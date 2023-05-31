open GoblintCil
open Pretty
open GobConfig
open FlagHelper

module M = Messages
module A = Array
module BI = IntOps.BigIntOps
module VDQ = ValueDomainQueries

type domain = TrivialDomain | PartitionedDomain | UnrolledDomain | NullByteDomain

(* determines the domain based on variable, type and flag *)
let get_domain ~varAttr ~typAttr =
  let domain_from_string = function
    | "partitioned" -> PartitionedDomain
    | "trivial" -> TrivialDomain
    | "unroll" ->  UnrolledDomain
    | _ -> failwith "AttributeConfiguredArrayDomain: invalid option for domain"
  in
  (*TODO add options?*)
  (*TODO let attribute determine unrolling factor?*)
  let from_attributes = List.find_map (
      fun (Attr (s,ps) )->
        if s = "goblint_array_domain" then (
          List.find_map (fun p -> match p with
              | AStr x -> Some x
              | _ -> None
            ) ps
        )
        else None
    ) in
  if get_bool "annotation.goblint_array_domain" then
    match from_attributes varAttr, from_attributes typAttr with
    | Some x, _ -> domain_from_string x
    | _, Some x -> domain_from_string x
    | _ -> domain_from_string @@ get_string "ana.base.arrays.domain"
  else domain_from_string @@ get_string "ana.base.arrays.domain"

let can_recover_from_top x = x <> TrivialDomain

module type S =
sig
  include Lattice.S
  type idx
  type value

  val domain_of_t: t -> domain

  val get: ?checkBounds:bool -> VDQ.t -> t -> Basetype.CilExp.t option * idx -> value
  val set: VDQ.t -> t -> Basetype.CilExp.t option * idx -> value -> t
  val make: ?varAttr:attributes -> ?typAttr:attributes -> idx -> value -> t
  val length: t -> idx option

  val move_if_affected: ?replace_with_const:bool -> VDQ.t -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
  val get_vars_in_e: t -> Cil.varinfo list
  val map: (value -> value) -> t -> t
  val fold_left: ('a -> value -> 'a) -> 'a -> t -> 'a
  val smart_join: (exp -> BI.t option) -> (exp -> BI.t option) -> t -> t -> t
  val smart_widen: (exp -> BI.t option) -> (exp -> BI.t option) -> t -> t -> t
  val smart_leq: (exp -> BI.t option) -> (exp -> BI.t option) -> t -> t -> bool
  val update_length: idx -> t -> t

  val project: ?varAttr:attributes -> ?typAttr:attributes -> VDQ.t -> t -> t
end

module type Str =
sig
  include S
  val to_string: t -> t
  val to_n_string: t -> int -> t
  val to_string_length: t -> idx
  val string_copy: t -> t -> int option -> t
  val string_concat: t -> t -> int option -> t
  val substring_extraction: t -> t -> t
  val string_comparison: t -> t -> int option -> idx
end

module type LatticeWithSmartOps =
sig
  include Lattice.S
  val smart_join: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> t
  val smart_widen: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> t
  val smart_leq: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> bool
end


module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  include Val
  let name () = "trivial arrays"
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = TrivialDomain

  let show x = "Array: " ^ Val.show x
  let pretty () x = text "Array: " ++ pretty () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let get ?(checkBounds=true) (ask: VDQ.t) a i = a
  let set (ask: VDQ.t) a i v = join a v
  let make ?(varAttr=[]) ?(typAttr=[])  i v = v
  let length _ = None

  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let get_vars_in_e _ = []
  let map f x = f x
  let fold_left f a x = f a x

  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq
  let update_length _ x = x

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t
end

let factor () =
  match get_int "ana.base.arrays.unrolling-factor" with
  | 0 -> failwith "ArrayDomain: ana.base.arrays.unrolling-factor needs to be set when using the unroll domain"
  | x -> x

module Unroll (Val: Lattice.S) (Idx:IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
struct
  module Factor = struct let x () = (get_int "ana.base.arrays.unrolling-factor") end
  module Base = Lattice.ProdList (Val) (Factor)
  include Lattice.ProdSimple(Base) (Val)

  let name () = "unrolled arrays"
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = UnrolledDomain

  let join_of_all_parts (xl, xr) = List.fold_left Val.join xr xl
  let show (xl, xr) =
    let rec show_list xlist = match xlist with
      | [] -> " --- "
      | hd::tl -> (Val.show hd ^ " - " ^ (show_list tl)) in
    "Array (unrolled to " ^ (Stdlib.string_of_int (factor ())) ^ "): " ^
    (show_list xl) ^ Val.show xr ^ ")"
  let pretty () x = text "Array: " ++ text (show x)
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let extract x default = match x with
    | Some c -> c
    | None -> default
  let get ?(checkBounds=true)  (ask: VDQ.t) (xl, xr) (_,i) =
    let search_unrolled_values min_i max_i =
      let rec subjoin l i = match l with
        | [] -> Val.bot ()
        | hd::tl ->
          begin
            match Z.gt i max_i, Z.lt i min_i with
            | false,true -> subjoin tl (Z.succ i)
            | false,false -> Val.join hd (subjoin tl (Z.succ i))
            | _,_ -> Val.bot ()
          end in
      subjoin xl Z.zero in
    let f = Z.of_int (factor ()) in
    let min_i = extract (Idx.minimal i) Z.zero in
    let max_i = extract (Idx.maximal i) f in
    if Z.geq min_i f then xr
    else if Z.lt max_i f then search_unrolled_values min_i max_i
    else Val.join xr (search_unrolled_values min_i (Z.of_int ((factor ())-1)))
  let set (ask: VDQ.t) (xl,xr) (_,i) v =
    let update_unrolled_values min_i max_i =
      let rec weak_update l i = match l with
        | [] -> []
        | hd::tl ->
          if Z.lt i min_i then hd::(weak_update tl (Z.succ i))
          else if Z.gt i max_i then (hd::tl)
          else (Val.join hd v)::(weak_update tl (Z.succ i)) in
      let rec full_update l i = match l with
        | [] -> []
        | hd::tl ->
          if Z.lt i min_i then hd::(full_update tl (Z.succ i))
          else v::tl in
      if Z.equal min_i max_i then full_update xl Z.zero
      else weak_update xl Z.zero in
    let f = Z.of_int (factor ()) in
    let min_i = extract(Idx.minimal i) Z.zero in
    let max_i = extract(Idx.maximal i) f in
    if Z.geq min_i f then (xl, (Val.join xr v))
    else if Z.lt max_i f then ((update_unrolled_values min_i max_i), xr)
    else ((update_unrolled_values min_i (Z.of_int ((factor ())-1))), (Val.join xr v))
  let make ?(varAttr=[]) ?(typAttr=[]) _ v =
    let xl = BatList.make (factor ()) v in
    (xl,Val.bot ())
  let length _ = None
  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let get_vars_in_e _ = []
  let map f (xl, xr) = ((List.map f xl), f xr)
  let fold_left f a x = f a (join_of_all_parts x)
  let printXml f (xl,xr) = BatPrintf.fprintf f "<value>\n<map>\n
  <key>unrolled array</key>\n
  <key>xl</key>\n%a\n\n
  <key>xm</key>\n%a\n\n
  </map></value>\n" Base.printXml xl Val.printXml xr
  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq
  let update_length _ x = x
  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t
end

(** Special signature so that we can use the _with_length functions from PartitionedWithLength but still match the interface *
  * defined for array domains *)
module type SPartitioned =
sig
  include S
  val set_with_length: idx option -> VDQ.t -> t -> Basetype.CilExp.t option * idx -> value -> t
  val smart_join_with_length: idx option -> (exp -> BI.t option) -> (exp -> BI.t option) -> t -> t -> t
  val smart_widen_with_length: idx option -> (exp -> BI.t option) -> (exp -> BI.t option)  -> t -> t-> t
  val smart_leq_with_length: idx option -> (exp -> BI.t option) -> (exp -> BI.t option) -> t -> t -> bool
  val move_if_affected_with_length: ?replace_with_const:bool -> idx option -> VDQ.t -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
end

module Partitioned (Val: LatticeWithSmartOps) (Idx:IntDomain.Z): SPartitioned with type value = Val.t and type idx = Idx.t =
struct
  include Printable.Std

  type t = Joint of Val.t | Partitioned of (CilType.Exp.t * (Val.t * Val.t * Val.t)) [@@deriving eq, ord, hash]

  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = PartitionedDomain

  let name () = "partitioned array"

  let relift = function
    | Joint v -> Joint (Val.relift v)
    | Partitioned (e, (l, m, r)) -> Partitioned (CilType.Exp.relift e, (Val.relift l, Val.relift m, Val.relift r))

  let join_of_all_parts = function
    | Joint v -> v
    | Partitioned (e, (xl, xm, xr)) -> Val.join xl (Val.join xm xr)

  (** Ensures an array where all three Val are equal, is represented by an unpartitioned array *)
  let normalize = function
    | Joint v -> Joint v
    | (Partitioned (e, (xl, xm, xr)) as p) ->
      if Val.equal xl xm && Val.equal xm xr then Joint xl
      else p

  let leq (x:t) (y:t) =
    match x, y with
    | Joint x, Joint y -> Val.leq x y
    | Partitioned (e,(xl, xm, xr)), Joint y -> Val.leq xl y && Val.leq xm y && Val.leq xr y
    | Partitioned (e,(xl, xm, xr)), Partitioned (e',(yl, ym, yr)) ->
      CilType.Exp.equal e e' && Val.leq xl yl && Val.leq xm ym && Val.leq xr yr
    | Joint x, Partitioned (e, (xl, xm, xr)) -> Val.leq x xl && Val.leq x xm && Val.leq x xr

  let bot () = Joint (Val.bot ())
  let is_bot (x:t) = Val.is_bot (join_of_all_parts x)
  let top () = Joint (Val.top ())
  let is_top = function
    | Joint x -> Val.is_top x
    | _-> false

  let join (x:t) (y:t) = normalize @@
    match x, y with
    | Joint x, Joint y -> Joint (Val.join x y)
    | Partitioned (e,(xl, xm, xr)), Joint y -> Partitioned (e,(Val.join xl y, Val.join xm y, Val.join xr y))
    | Joint x, Partitioned (e,(yl, ym, yr)) -> Partitioned (e,(Val.join x yl, Val.join x ym, Val.join x yr))
    | Partitioned (e,(xl, xm, xr)), Partitioned (e',(yl, ym, yr)) ->
      if CilType.Exp.equal e e' then Partitioned (e,(Val.join xl yl, Val.join xm ym, Val.join xr yr))
      else Joint (Val.join (join_of_all_parts x) (join_of_all_parts y))

  let widen (x:t) (y:t) = normalize @@ match x,y with
    | Joint x, Joint y -> Joint (Val.widen x y)
    | Partitioned (e,(xl, xm, xr)), Joint y -> Partitioned (e,(Val.widen xl y, Val.widen xm y, Val.widen xr y))
    | Joint x, Partitioned (e,(yl, ym, yr)) -> Partitioned (e,(Val.widen x yl, Val.widen x ym, Val.widen x yr))
    | Partitioned (e,(xl, xm, xr)), Partitioned (e',(yl, ym, yr)) ->
      if CilType.Exp.equal e e' then Partitioned (e,(Val.widen xl yl, Val.widen xm ym, Val.widen xr yr))
      else Joint (Val.widen (join_of_all_parts x) (join_of_all_parts y))

  let show = function
    | Joint x ->  "Array (no part.): " ^ Val.show x
    | Partitioned (e,(xl, xm, xr)) ->
      "Array (part. by " ^ CilType.Exp.show e ^ "): (" ^
      Val.show xl ^ " -- " ^
      Val.show xm ^ " -- " ^
      Val.show xr ^ ")"

  let pretty () x = text "Array: " ++ text (show x)
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let printXml f = function
    | Joint x -> BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
    | Partitioned (e,(xl, xm, xr)) ->
      BatPrintf.fprintf f "<value>\n<map>\n
          <key>Partitioned By</key>\n%a\n
          <key>l</key>\n%a\n\n
          <key>m</key>\n%a\n\n
          <key>r</key>\n%a\n\n
        </map></value>\n" CilType.Exp.printXml e Val.printXml xl Val.printXml xm Val.printXml xr

  let to_yojson = function
    | Joint x -> `Assoc [ ("any", Val.to_yojson x) ]
    | Partitioned (e,(xl, xm, xr)) ->
      `Assoc [ ("partitioned_by", CilType.Exp.to_yojson e);
               ("l", Val.to_yojson xl);
               ("m", Val.to_yojson xm);
               ("r", Val.to_yojson xr) ]

  let get ?(checkBounds=true) (ask:VDQ.t) (x:t) (i,_) =
    match x, i with
    | Joint v, _ -> v
    | Partitioned (e, (xl, xm, xr)), Some i' ->
      begin
        if VDQ.must_be_equal ask.eval_int e i' then xm
        else
          begin
            let contributionLess = match VDQ.may_be_less ask.eval_int i' e with        (* (may i < e) ? xl : bot *)
              | false -> Val.bot ()
              | _ -> xl in
            let contributionEqual = match VDQ.may_be_equal ask.eval_int i' e with      (* (may i = e) ? xm : bot *)
              | false -> Val.bot ()
              | _ -> xm in
            let contributionGreater =  match VDQ.may_be_less ask.eval_int e i' with    (* (may i > e) ? xr : bot *)
              | false -> Val.bot ()
              | _ -> xr in
            Val.join (Val.join contributionLess contributionEqual) contributionGreater
          end
      end
    | _ -> join_of_all_parts x

  let get_vars_in_e = function
    | Partitioned (e, _) -> Basetype.CilExp.get_vars e
    | _ -> []

  (* expressions containing globals or array accesses are not suitable for partitioning *)
  let not_allowed_for_part e =
    let rec contains_array_access e =
      let rec offset_contains_array_access offs =
        match offs with
        | NoOffset -> false
        | Index _ -> true
        | Field (_, o) -> offset_contains_array_access o
      in
      match e with
      |	Const _
      |	SizeOf _
      |	SizeOfE _
      |	SizeOfStr _
      |	AlignOf _
      |	AlignOfE _ -> false
      | Question(e1, e2, e3, _) ->
        contains_array_access e1 || contains_array_access e2 || contains_array_access e3
      |	CastE(_, e)
      |	UnOp(_, e , _)
      | Real e
      | Imag e -> contains_array_access e
      |	BinOp(_, e1, e2, _) -> contains_array_access e1 || contains_array_access e2
      | AddrOf _
      | AddrOfLabel _
      | StartOf _ -> false
      | Lval(Mem e, o) -> offset_contains_array_access o || contains_array_access e
      | Lval(Var _, o) -> offset_contains_array_access o
    in
    let vars = Basetype.CilExp.get_vars e in
    List.exists (fun x -> x.vglob) vars || contains_array_access e


  let map f = function
    | Joint v -> Joint (f v)
    | Partitioned (e,(xl, xm, xr)) -> normalize @@ Partitioned (e,(f xl, f xm, f xr))

  let fold_left f a = function
    | Joint x -> f a x
    | Partitioned (_, (xl,xm,xr)) -> f (f (f a xl) xm) xr

  let move_if_affected_with_length ?(replace_with_const=false) length (ask:VDQ.t) x (v:varinfo) (movement_for_exp: exp -> int option) =
    normalize @@
    let move (i:int option) (e, (xl,xm, xr)) =
      match i with
      | Some 0   ->
        Partitioned (e, (xl, xm, xr))
      | Some 1   ->
        Partitioned (e, (Val.join xl xm, xr, xr)) (* moved one to the right *)
      | Some -1  ->
        Partitioned (e, (xl, xl, Val.join xm xr)) (* moved one to the left  *)
      | Some x when x > 1 ->
        Partitioned (e, (Val.join (Val.join xl xm) xr, xr, xr)) (* moved more than one to the right *)
      | Some x when x < -1 ->
        Partitioned (e, (xl, xl, Val.join (Val.join xl xm) xr)) (* moved more than one to the left *)
      | _ ->
        begin
          let nval = join_of_all_parts x in
          let default = Joint nval in
          if replace_with_const then
            let n = ask.eval_int e in
            match VDQ.ID.to_int n with
            | Some i ->
              Partitioned ((Cil.kintegerCilint (Cilfacade.ptrdiff_ikind ()) i), (xl, xm, xr))
            | _ -> default
          else
            default
        end
    in
    match x with
    | Partitioned (e, (xl,xm, xr)) ->
      let is_affected = Basetype.CilExp.occurs v e in
      if not is_affected then
        x
      else
        (* check if one part covers the entire array, so we can drop partitioning *)
        begin
          let e_must_bigger_max_index =
            match length with
            | Some l ->
              begin
                match Idx.to_int l with
                | Some i ->
                  let b = VDQ.may_be_less ask.eval_int e (Cil.kintegerCilint (Cilfacade.ptrdiff_ikind ()) i) in
                  not b (* !(e <_{may} length) => e >=_{must} length *)
                | None -> false
              end
            | _ -> false
          in
          let e_must_less_zero =
            VDQ.eval_int_binop (module BoolDomain.MustBool) Lt ask.eval_int e Cil.zero (* TODO: untested *)
          in
          if e_must_bigger_max_index then
            (* Entire array is covered by left part, dropping partitioning. *)
            Joint xl
          else if e_must_less_zero then
            (* Entire array is covered by right value, dropping partitioning. *)
            Joint xr
          else
            (* If we can not drop partitioning, move *)
            move (movement_for_exp e) (e, (xl,xm, xr))
        end
    | _ -> x (* If the array is not partitioned, nothing to do *)

  let move_if_affected ?replace_with_const = move_if_affected_with_length ?replace_with_const None

  let set_with_length length (ask:VDQ.t) x (i,_) a =
    if M.tracing then M.trace "update_offset" "part array set_with_length %a %s %a\n" pretty x (BatOption.map_default Basetype.CilExp.show "None" i) Val.pretty a;
    if i = Some MyCFG.all_array_index_exp then
      (assert !Goblintutil.global_initialization; (* just joining with xm here assumes that all values will be set, which is guaranteed during inits *)
       (* the join is needed here! see e.g 30/04 *)
       let o = match x with Partitioned (_, (_, xm, _)) -> xm | Joint v -> v in
       let r =  Val.join o a in
       Joint r)
    else
      normalize @@
      let use_last = get_string "ana.base.partition-arrays.keep-expr" = "last" in
      let exp_value e =
        let n = ask.eval_int e in
        Option.map BI.of_bigint (VDQ.ID.to_int n)
      in
      let equals_zero e = BatOption.map_default (BI.equal BI.zero) false (exp_value e) in
      let equals_maxIndex e =
        match length with
        | Some l ->
          begin
            match Idx.to_int l with
            | Some i -> BatOption.map_default (BI.equal (BI.sub i BI.one)) false (exp_value e)
            | None -> false
          end
        | _ -> false
      in
      let lubIfNotBot x = if Val.is_bot x then x else Val.join a x in
      match x with
      | Joint v ->
        (match i with
         | Some i when not @@ not_allowed_for_part i ->
           let l = if equals_zero i then Val.bot () else join_of_all_parts x in
           let r = if equals_maxIndex i then Val.bot () else join_of_all_parts x in
           Partitioned (i, (l, a, r))
         | _ -> Joint (Val.join v a)
        )
      | Partitioned (e, (xl, xm, xr)) ->
        let isEqual = VDQ.must_be_equal ask.eval_int in
        match i with
        | Some i' when not use_last || not_allowed_for_part i' -> begin
            let default =
              let left =
                match VDQ.may_be_less ask.eval_int i' e with     (* (may i < e) ? xl : bot *) (* TODO: untested *)
                | false -> xl
                | _ -> lubIfNotBot xl in
              let middle =
                match VDQ.may_be_equal ask.eval_int i' e with    (* (may i = e) ? xm : bot *)
                | false -> xm
                | _ -> Val.join xm a in
              let right =
                match VDQ.may_be_less ask.eval_int e i' with     (* (may i > e) ? xr : bot *) (* TODO: untested *)
                | false -> xr
                | _ -> lubIfNotBot xr in
              Partitioned (e, (left, middle, right))
            in
            if isEqual e i' then
              (*  e = _{must} i => update strongly *)
              Partitioned (e, (xl, a, xr))
            else if Cil.isConstant e && Cil.isConstant i' then
              match Cil.getInteger e, Cil.getInteger i' with
              | Some (e'': Cilint.cilint), Some i'' ->
                if BI.equal  i'' (BI.add e'' BI.one) then
                  (* If both are integer constants and they are directly adjacent, we change partitioning to maintain information *)
                  Partitioned (i', (Val.join xl xm, a, xr))
                else if BI.equal e'' (BI.add i'' BI.one) then
                  Partitioned (i', (xl, a, Val.join xm xr))
                else
                  default
              | _ ->
                default
            else
              default
          end
        | Some i' ->
          if isEqual e i' then
            Partitioned (e, (xl, a, xr))
          else
            let left = if equals_zero i' then Val.bot () else Val.join xl @@ Val.join
                  (match VDQ.may_be_equal ask.eval_int e i' with (* TODO: untested *)
                   | false -> Val.bot()
                   | _ -> xm) (* if e' may be equal to i', but e' may not be smaller than i' then we only need xm *)
                  (
                    let t = Cilfacade.typeOf e in
                    let ik = Cilfacade.get_ikind t in
                    match VDQ.must_be_equal ask.eval_int (BinOp(PlusA, e, Cil.kinteger ik 1, t)) i' with
                    | true -> xm
                    | _ ->
                      begin
                        match VDQ.may_be_less ask.eval_int e i' with (* TODO: untested *)
                        | false-> Val.bot()
                        | _ -> Val.join xm xr (* if e' may be less than i' then we also need xm for sure *)
                      end
                  )
            in
            let right = if equals_maxIndex i' then Val.bot () else  Val.join xr @@  Val.join
                  (match VDQ.may_be_equal ask.eval_int e i' with (* TODO: untested *)
                   | false -> Val.bot()
                   | _ -> xm)

                  (
                    let t = Cilfacade.typeOf e in
                    let ik = Cilfacade.get_ikind t in
                    match VDQ.must_be_equal ask.eval_int (BinOp(PlusA, e, Cil.kinteger ik (-1), t)) i' with (* TODO: untested *)
                    | true -> xm
                    | _ ->
                      begin
                        match VDQ.may_be_less ask.eval_int i' e with (* TODO: untested *)
                        | false -> Val.bot()
                        | _ -> Val.join xl xm (* if e' may be less than i' then we also need xm for sure *)
                      end
                  )
            in
            (* The new thing is partitioned according to i so we can strongly update *)
            Partitioned (i',(left, a, right))
        | _ ->
          (* If the expression used to write is not known, all segments except the empty ones will be affected *)
          Partitioned (e, (lubIfNotBot xl, Val.join xm a, lubIfNotBot xr))

  let set = set_with_length None


  let make ?(varAttr=[]) ?(typAttr=[]) i v:t =
    if Idx.to_int i = Some BI.one  then
      Partitioned ((Cil.integer 0), (v, v, v))
    else if Val.is_bot v then
      Joint (Val.bot ())
    else
      Joint v

  let length _ = None

  let smart_op (op: Val.t -> Val.t -> Val.t) length x1 x2 x1_eval_int x2_eval_int =
    normalize @@
    let must_be_length_minus_one v = match length with
      | Some l ->
        begin
          match Idx.to_int l with
          | Some i ->
            v = Some (BI.sub i BI.one)
          | None -> false
        end
      | None -> false
    in
    let must_be_zero v = v = Some BI.zero in
    let op_over_all = op (join_of_all_parts x1) (join_of_all_parts x2) in
    match x1, x2 with
    | Partitioned (e1, (xl1, xm1, xr1)), Partitioned (e2, (xl2, xm2, xr2)) when Basetype.CilExp.equal e1 e2 ->
      Partitioned (e1, (op xl1 xl2, op xm1 xm2, op xr1 xr2))
    | Partitioned (e1, (xl1, xm1, xr1)), Partitioned (e2, (xl2, xm2, xr2)) ->
      if get_string "ana.base.partition-arrays.keep-expr" = "last" || get_bool "ana.base.partition-arrays.smart-join" then
        let op = Val.join in (* widen between different components isn't called validly *)
        let over_all_x1 = op (op xl1 xm1) xr1 in
        let over_all_x2 = op (op xl2 xm2) xr2 in
        let e1_in_state_of_x2 = x2_eval_int e1 in
        let e2_in_state_of_x1 = x1_eval_int e2 in
        (* TODO: why does this depend on exp comparison? probably to use "simpler" expression according to constructor order in compare *)
        (* It is mostly SOME order to ensure commutativity of join *)
        let e1_is_better = (not (Cil.isConstant e1) && Cil.isConstant e2) || Basetype.CilExp.compare e1 e2 < 0 in
        if e1_is_better then (* first try if the result can be partitioned by e1e *)
          if must_be_zero e1_in_state_of_x2  then
            Partitioned (e1, (xl1, op xm1 over_all_x2, op xr1 over_all_x2))
          else if must_be_length_minus_one e1_in_state_of_x2  then
            Partitioned (e1, (op xl1 over_all_x2, op xm1 over_all_x2, xr1))
          else if must_be_zero e2_in_state_of_x1 then
            Partitioned (e2, (xl2, op over_all_x1 xm2, op over_all_x1 xr2))
          else if must_be_length_minus_one e2_in_state_of_x1 then
            Partitioned (e2, (op over_all_x1 xl2, op over_all_x1 xm2, xr2))
          else
            Joint op_over_all
        else  (* first try if the result can be partitioned by e2e *)
        if must_be_zero e2_in_state_of_x1 then
          Partitioned (e2, (xl2, op over_all_x1 xm2, op over_all_x1 xr2))
        else if must_be_length_minus_one e2_in_state_of_x1 then
          Partitioned (e2, (op over_all_x1 xl2, op over_all_x1 xm2, xr2))
        else if must_be_zero e1_in_state_of_x2 then
          Partitioned (e1, (xl1, op xm1 over_all_x2, op xr1 over_all_x2))
        else if must_be_length_minus_one e1_in_state_of_x2 then
          Partitioned (e1, (op xl1 over_all_x2, op xm1 over_all_x2, xr1))
        else
          Joint op_over_all
      else
        Joint op_over_all
    | Joint _, Joint _ ->
      Joint op_over_all
    | Joint x1, Partitioned (e2, (xl2, xm2, xr2)) ->
      if must_be_zero (x1_eval_int e2) then
        Partitioned (e2, (xl2, op x1 xm2, op x1 xr2))
      else if must_be_length_minus_one (x1_eval_int e2) then
        Partitioned (e2, (op x1 xl2, op x1 xm2, xr2))
      else
        Joint op_over_all
    | Partitioned (e1, (xl1, xm1, xr1)), Joint x2 ->
      if must_be_zero (x2_eval_int e1) then
        Partitioned (e1, (xl1, op xm1 x2, op xr1 x2))
      else if must_be_length_minus_one (x2_eval_int e1) then
        Partitioned (e1, (op xl1 x2, op xm1 x2, xr1))
      else
        Joint op_over_all

  let smart_join_with_length length x1_eval_int x2_eval_int x1 x2 =
    smart_op (Val.smart_join x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_widen_with_length length x1_eval_int x2_eval_int x1 x2  =
    smart_op (Val.smart_widen x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_leq_with_length length x1_eval_int x2_eval_int x1 x2 =
    let leq' = Val.smart_leq x1_eval_int x2_eval_int in
    let must_be_zero v = (v = Some BI.zero) in
    let must_be_length_minus_one v =  match length with
      | Some l ->
        begin
          match Idx.to_int l with
          | Some i ->
            v = Some (BI.sub i BI.one)
          | None -> false
        end
      | None -> false
    in
    match x1, x2 with
    | Joint x1, Joint x2 ->
      leq' x1 x2
    | Partitioned (e1, (xl1, xm1, xr1)), Joint x2 ->
      (* leq' (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2)) *)
      leq' xl1 x2 && leq' xm1 x2 && leq' xr1 x2
    | Partitioned (e1, (xl1, xm1, xr1)), Partitioned (e2, (xl2, xm2, xr2)) ->
      if Basetype.CilExp.equal e1 e2 then
        leq' xl1 xl2 && leq' xm1 xm2 && leq' xr1 xr2
      else if must_be_zero (x1_eval_int e2) then
        (* A read will never be from xl2 -> we can ignore that here *)
        let l = join_of_all_parts x1 in
        leq' l xm2 && leq' l xr2
      else if must_be_length_minus_one (x1_eval_int e2) then
        (* A read will never be from xr2 -> we can ignore that here *)
        let l = join_of_all_parts x1 in
        leq' l xl2 && leq' l xm2
      else
        false
    | Joint x1, Partitioned (e2, (xl2, xm2, xr2)) ->
      if must_be_zero (x1_eval_int e2) then
        leq' x1 xm2 && leq' x1 xr2
      else if must_be_length_minus_one (x1_eval_int e2) then
        leq' x1 xl2 && leq' x1 xm2
      else
        leq' x1 xl2 && leq' x1 xr2 && leq' x1 xm2 && leq' x1 xr2

  let smart_join = smart_join_with_length None
  let smart_widen = smart_widen_with_length None
  let smart_leq = smart_leq_with_length None

  let meet x y = normalize @@ match x,y with
    | Joint x, Joint y -> Joint (Val.meet x y)
    | Joint x, Partitioned (e, (xl, xm, xr))
    | Partitioned (e, (xl, xm, xr)), Joint x ->
      Partitioned (e, (Val.meet x xl, Val.meet x xm, Val.meet x xr))
    | Partitioned (e1, (xl1, xm1, xr1)), Partitioned (e2, (xl2, xm2, xr2)) ->
      if Basetype.CilExp.equal e1 e2 then
        Partitioned (e1, (Val.meet xl1 xl2, Val.meet xm1 xm2, Val.meet xr1 xr2))
      else
        (* partitioned according to two different expressions -> meet can not be element-wise *)
        (* arrays can not be partitioned according to multiple expressions, arbitrary prefer the first one here *)
        (* TODO: do smart things if the relationship between e1e and e2e is known *)
        x

  let narrow x y = normalize @@ match x,y with
    | Joint x, Joint y -> Joint (Val.narrow x y)
    | Joint x, Partitioned (e, (xl, xm, xr))
    | Partitioned (e, (xl, xm, xr)), Joint x ->
      Partitioned (e, (Val.narrow x xl, Val.narrow x xm, Val.narrow x xr))
    | Partitioned (e1, (xl1, xm1, xr1)), Partitioned (e2, (xl2, xm2, xr2)) ->
      if Basetype.CilExp.equal e1 e2 then
        Partitioned (e1, (Val.narrow xl1 xl2, Val.narrow xm1 xm2, Val.narrow xr1 xr2))
      else
        (* partitioned according to two different expressions -> narrow can not be element-wise *)
        (* arrays can not be partitioned according to multiple expressions, arbitrary prefer the first one here *)
        x

  let update_length _ x = x
  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t
end

(* This is the main array out of bounds check *)
let array_oob_check ( type a ) (module Idx: IntDomain.Z with type t = a) (x, l) (e, v) =
  if GobConfig.get_bool "ana.arrayoob" then (* The purpose of the following 2 lines is to give the user extra info about the array oob *)
    let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
    and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong BI.zero)) in (* check whether the index is non-negative *)
    (* For an explanation of the warning types check the Pull Request #255 *)
    match(idx_after_start, idx_before_end) with
    | Some true, Some true -> (* Certainly in bounds on both sides.*)
      ()
    | Some true, Some false -> (* The following matching differentiates the must and may cases*)
      M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Must access array past end"
    | Some true, None ->
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "May access array past end"
    | Some false, Some true ->
      M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "Must access array before start"
    | None, Some true ->
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "May access array before start"
    | _ ->
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.unknown "May access array out of bounds"
  else ()


module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Trivial (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = TrivialDomain

  let get ?(checkBounds=true) (ask : VDQ.t) (x, (l : idx)) (e, v) =
    if checkBounds then (array_oob_check (module Idx) (x, l) (e, v));
    Base.get ask x (e, v)
  let set (ask: VDQ.t) (x,l) i v = Base.set ask x i v, l
  let make ?(varAttr=[]) ?(typAttr=[])  l x = Base.make l x, l
  let length (_,l) = Some l
  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let get_vars_in_e _ = []

  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq

  (* It is not necessary to do a least-upper bound between the old and the new length here.   *)
  (* Any array can only be declared in one location. The value for newl that we get there is  *)
  (* the one obtained by abstractly evaluating the size expression at this location for the   *)
  (* current state. If newl leq l this means that we somehow know more about the expression   *)
  (* determining the size now (e.g. because of narrowing), but this holds for all the times   *)
  (* the declaration is visited. *)
  let update_length newl (x, l) = (x, newl)

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y

  let to_yojson (x, y) = `Assoc [ (Base.name (), Base.to_yojson x); ("length", Idx.to_yojson y) ]
end


module PartitionedWithLength (Val: LatticeWithSmartOps) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Partitioned (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = PartitionedDomain

  let get ?(checkBounds=true) (ask : VDQ.t) (x, (l : idx)) (e, v) =
    if checkBounds then (array_oob_check (module Idx) (x, l) (e, v));
    Base.get ask x (e, v)
  let set ask (x,l) i v = Base.set_with_length (Some l) ask x i v, l
  let make ?(varAttr=[]) ?(typAttr=[])  l x = Base.make l x, l
  let length (_,l) = Some l

  let move_if_affected ?replace_with_const ask (x,l) v i =
    (Base.move_if_affected_with_length ?replace_with_const (Some l) ask x v i), l

  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let get_vars_in_e (x, _) = Base.get_vars_in_e x

  let smart_join x_eval_int y_eval_int (x,xl) (y,yl) =
    let l = Idx.join xl yl in
    (Base.smart_join_with_length (Some l) x_eval_int y_eval_int x y , l)

  let smart_widen x_eval_int y_eval_int (x,xl) (y,yl) =
    let l = Idx.join xl yl in
    (Base.smart_widen_with_length (Some l) x_eval_int y_eval_int x y, l)

  let smart_leq x_eval_int y_eval_int (x,xl) (y,yl)  =
    let l = Idx.join xl yl in
    Idx.leq xl yl && Base.smart_leq_with_length (Some l) x_eval_int y_eval_int x y

  (* It is not necessary to do a least-upper bound between the old and the new length here.   *)
  (* Any array can only be declared in one location. The value for newl that we get there is  *)
  (* the one obtained by abstractly evaluating the size expression at this location for the   *)
  (* current state. If newl leq l this means that we somehow know more about the expression   *)
  (* determining the size now (e.g. because of narrowing), but this holds for all the times   *)
  (* the declaration is visited. *)
  let update_length newl (x, l) = (x, newl)

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y

  let to_yojson (x, y) = `Assoc [ (Base.name (), Base.to_yojson x); ("length", Idx.to_yojson y) ]
end

module UnrollWithLength (Val: Lattice.S) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Unroll (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = UnrolledDomain

  let get ?(checkBounds=true) (ask : VDQ.t) (x, (l : idx)) (e, v) =
    if checkBounds then (array_oob_check (module Idx) (x, l) (e, v));
    Base.get ask x (e, v)
  let set (ask: VDQ.t) (x,l) i v = Base.set ask x i v, l
  let make ?(varAttr=[]) ?(typAttr=[]) l x = Base.make l x, l
  let length (_,l) = Some l

  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let get_vars_in_e _ = []

  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq

  (* It is not necessary to do a least-upper bound between the old and the new length here.   *)
  (* Any array can only be declared in one location. The value for newl that we get there is  *)
  (* the one obtained by abstractly evaluating the size expression at this location for the   *)
  (* current state. If newl leq l this means that we somehow know more about the expression   *)
  (* determining the size now (e.g. because of narrowing), but this holds for all the times   *)
  (* the declaration is visited. *)
  let update_length newl (x, l) = (x, newl)

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y

  let to_yojson (x, y) = `Assoc [ (Base.name (), Base.to_yojson x); ("length", Idx.to_yojson y) ]
end

module type LatticeWithNull =
sig
  include LatticeWithSmartOps
  val null: unit -> t
  val not_null: unit -> t
  val is_null: t -> bool
end

module NullByte (Val: LatticeWithNull) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
struct
  module MustNulls = SetDomain.Reverse (SetDomain.ToppedSet (IntDomain.BigInt) (struct let topname = "No Nulls" end))
  module MayNulls = SetDomain.ToppedSet (IntDomain.BigInt) (struct let topname = "All Null" end)
  (* (Must Null Set, May Null Set, Array Size) *)
  include Lattice.Prod3 (MustNulls) (MayNulls) (Idx)

  let name () = "arrays containing null bytes"
  type idx = Idx.t
  type value = Val.t

  let domain_of_t _ = NullByteDomain

  let get ?(checkBounds=true) (ask: VDQ.t) (must_nulls_set, _, size) (e, i) =
    let rec all_indexes_must_null i max =
      if Z.gt i max then
        true
      else if MustNulls.exists (Z.equal i) must_nulls_set then
        all_indexes_must_null (Z.add i Z.one) max
      else
        false in
    let min_i = match Idx.minimal i with
      | Some min -> 
        if Z.lt min Z.zero then
          Z.zero (* assume worst case minimal index *)
        else
          min
      | None -> Z.zero in (* assume worst case minimal index *)
    let max_i = Idx.maximal i in

    (* warn if index is (potentially) out of bounds *)
    if checkBounds then (array_oob_check (module Idx) (must_nulls_set, size) (e, i));
    match max_i, Idx.minimal size with
    (* if there is no maximum number in interval, return top of value *)
    | None, _ -> Val.top ()
    | Some max, Some min_size when Z.geq max Z.zero && Z.lt max min_size ->
      (* else only return null if all numbers in interval are in must null index set *)
      if all_indexes_must_null min_i max then 
        Val.null ()
      else
        Val.top ()
    (* if maximum number in interval is invalid, i.e. negative, return top of value *)
    | _ -> Val.top ()

  let set (ask: VDQ.t) (must_nulls_set, may_nulls_set, size) (e, i) v =
    let rec add_indexes i max may_nulls_set =
      if Z.gt i max then
        may_nulls_set
      else
        add_indexes (Z.add i Z.one) max (MayNulls.add i may_nulls_set) in
    let rec remove_indexes i max must_nulls_set =
      if Z.gt i max then
        may_nulls_set
      else
        remove_indexes (Z.add i Z.one) max (MustNulls.remove i must_nulls_set) in
    let min_of_natural_number num =
      match Idx.minimal num with
      | Some min ->
        if Z.lt min Z.zero then
          Z.zero (* assume worst case minimal index *)
        else
          min
      | None -> Z.zero in (* assume worst case moptionimal index *)
    let min_size = min_of_natural_number size in
    let min_i = min_of_natural_number i in
    let max_i = Idx.maximal i in

    (* warn if index is (potentially) out of bounds *)
    array_oob_check (module Idx) (must_nulls_set, size) (e, i);
    match max_i, Val.is_null v with
    (* if no maximum number in interval and value = null, modify may_nulls_set to top = all possible indexes < size *)
    | None, true -> (must_nulls_set, MayNulls.top (), size)
    (* if no maximum number in interval and value != null, modify must_nulls_set to top = empty set *)
    | None, false -> (MustNulls.top (), may_nulls_set, size)
    (* if value = null *)
    | Some max, true when Z.geq max Z.zero ->
      begin match Idx.maximal size with
        | Some max_size ->
          (* ... and i is exact number < size, add i to must_nulls_set and may_nulls_set *)
          if Z.equal min_i max && Z.lt min_i min_size then
            (MustNulls.add min_i must_nulls_set, MayNulls.add min_i may_nulls_set, size)
          (* ... and i is exact number in size interval, add i only to may_nulls_set *)
          else if Z.equal min_i max && Z.lt min_i max_size then
            (must_nulls_set, MayNulls.add min_i may_nulls_set, size)
          (* ... and i is exact number >= size, warn and return tuple unmodified *)
          else if Z.equal min_i max then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (must_nulls_set, may_nulls_set, size))
          (* ... and i is interval with lower bound = 0 and upper bound in size interval, modify may_nulls_set to top *)
          else if Z.equal min_i Z.zero && Z.equal max (Z.sub max_size Z.one) then
            (must_nulls_set, MayNulls.top (), size)
          (* ... and i is interval with lower bound = 0 and upper bound >= size, warn and modify may_nulls_set to top *)
          else if Z.equal min_i Z.zero && Z.geq max max_size then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (must_nulls_set, MayNulls.top (), size))
          (* ... and i is interval with lower bound > 0 and upper bound >= size, warn and add all indexes from interval lower bound to size to may_nulls_set *)
          else if Z.geq max max_size then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (must_nulls_set, add_indexes min_i max_size may_nulls_set, size))
          (* ... and i is interval with upper bound < size, add all indexes of interval to may_nulls_set*)
          else
            (must_nulls_set, add_indexes min_i max may_nulls_set, size)
        (* ..., size has no upper limit *)
        | None ->
          (* ... and i is exact number < minimal size, add i to must_nulls_set and may_nulls_set *)
          if Z.equal min_i max && Z.lt min_i min_size then
            (MustNulls.add min_i must_nulls_set, MayNulls.add min_i may_nulls_set, size)
          (* ... and i is exact number >= minimal size, add i to may_nulls_set only *)
          else if Z.equal min_i max then
            (must_nulls_set, MayNulls.add min_i may_nulls_set, size)
          (* ... and i is interval, add all indexes of interval to may_nulls_set *)
          else
            (must_nulls_set, add_indexes min_i max may_nulls_set, size)
      end
    (* if value != null *)
    | Some max, false when Z.geq max Z.zero ->
      begin match Idx.maximal size with
        | Some max_size ->
          (* ... and i is exact number < size, remove i from must_nulls_set and may_nulls_set *)
          if Z.equal min_i max && Z.lt min_i min_size then
            (MustNulls.remove min_i must_nulls_set, MayNulls.remove min_i may_nulls_set, size)
          (* ... and i is exact number in size interval, remove i only from must_nulls_set *)
          else if Z.equal min_i max && Z.lt min_i max_size then
            (MustNulls.remove min_i must_nulls_set, may_nulls_set, size)
          (* ... and i is exact number >= size, warn and return tuple unmodified *)
          else if Z.equal min_i max then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (must_nulls_set, may_nulls_set, size))
          (* ... and i is interval with lower bound = 0 and upper bound = size, modify must_nulls_set to top *)
          else if Z.equal min_i Z.zero && Z.equal max max_size then
            (MustNulls.top (), may_nulls_set, size)
          (* ... and i is interval with lower bound = 0 and upper bound >= size, warn and modify must_nulls_set to top *)
          else if Z.equal min_i Z.zero && Z.geq max max_size then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (MustNulls.top (), may_nulls_set, size))
          (* ... and i is interval with lower bound > 0 and upper bound >= size, warn and remove all indexes from interval lower bound to size from must_nulls_set *)
          else if Z.geq max max_size then
            (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Write operation outside of array bounds";
            (remove_indexes min_i max_size must_nulls_set, may_nulls_set, size))
          (* ... and i is interval with upper bound < size, remove all indexes of interval from must_nulls_set *)
          else
            (remove_indexes min_i max must_nulls_set, may_nulls_set, size)
        (* ..., size is unlimited *)
        | None ->
          (* ... and i is exact number < minimal size, remove i from must_nulls_set and may_nulls_set *)
          if Z.equal min_i max && Z.lt min_i min_size then
            (MustNulls.remove min_i must_nulls_set, MayNulls.remove min_i may_nulls_set, size)
          (* ... and i is exact number >= minimal size, remove i from must_nulls_set only *)
          else if Z.equal min_i max then
            (MustNulls.remove min_i must_nulls_set, may_nulls_set, size)
          (* ... and i is interval, remove all indexes from interval of must_nulls_set *)
          else
            (remove_indexes min_i max must_nulls_set, may_nulls_set, size)
      end
    (* if maximum number in interval is invalid, i.e. negative, return tuple unmodified *)
    | _ -> (must_nulls_set, may_nulls_set, size)

  let make ?(varAttr=[]) ?(typAttr=[]) i v =
    let min_i, max_i = match Idx.minimal i, Idx.maximal i with
      | Some min, Some max ->
        if Z.lt min Z.zero && Z.lt max Z.zero then
          (M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "Tries to create an array of negative size";
          Z.zero, Some Z.zero)
        else if Z.lt min Z.zero then
          (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "May try to create an array of negative size";
          Z.zero, Some max)
        else
          min, Some max
      | None, Some max ->
        if Z.lt max Z.zero then
          (M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "Tries to create an array of negative size";
          Z.zero, Some Z.zero)
        else
          Z.zero, Some max
      | Some min, None ->
        if Z.lt min Z.zero then
          (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "May try to create an array of negative size";
          Z.zero, None)
        else
          min, None
      | None, None -> Z.zero, None in
    match max_i, Val.is_null v with
    (* if value = null, return (bot = all indexes up to minimal size - 1, top = all indexes up to maximal size - 1, size) *)
    | Some max, true -> (MustNulls.bot (), MayNulls.top (), Idx.of_interval !Cil.kindOfSizeOf (min_i, max))
    | None, true -> (MustNulls.bot (), MayNulls.top (), Idx.starting !Cil.kindOfSizeOf min_i)
    (* if value != null, return (top = no indexes, bot = no indexes, size) *)
    | Some max, false -> (MustNulls.top (), MayNulls.bot (), Idx.of_interval !Cil.kindOfSizeOf (min_i, max))
    | None, false -> (MustNulls.top (), MayNulls.bot (), Idx.starting !Cil.kindOfSizeOf min_i)

  let length (_, _, size) = Some size

  let move_if_affected ?(replace_with_const=false) _ sets_and_size _ _ = sets_and_size

  let get_vars_in_e _ = []

  let map f (must_nulls_set, may_nulls_set, size) =
    (* if f(null) = null, all values in must_nulls_set still are surely null; 
     * assume top for may_nulls_set as checking effect of for every possible value is unfeasbile*)
    if Val.is_null (f (Val.null ())) then
      (must_nulls_set, MayNulls.top (), size)
    (* else also return top for must_nulls_set *)
    else
      (MustNulls.top (), MayNulls.top (), size)

  (* TODO: check there is no smarter implementation -- problem is domain doesn't work on values but Z.t / idx for size *)
  let fold_left f acc _ = f acc (Val.top ())
  
  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq

  (* string functions *)
  let to_string (must_nulls_set, may_nulls_set, size) =
    (* if must_nulls_set and min_nulls_set empty, definitely no null byte in array => warn about certain buffer overflow and return tuple unchanged *)
    if MustNulls.is_empty must_nulls_set && MayNulls.is_empty may_nulls_set then
      (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Array access past end: buffer overflow";
      (must_nulls_set, may_nulls_set, size))
    (* if only must_nulls_set empty, no certainty about array containing null byte => warn about potential buffer overflow and return tuple unchanged *)
    else if MustNulls.is_empty must_nulls_set then
      (M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "May access array past end: potential buffer overflow";
      must_nulls_set, may_nulls_set, size)
    else
      let min_must_null = MustNulls.min_elt must_nulls_set in
      (* if smallest index in sets coincides, only this null byte is kept in both sets *)
      if Z.equal min_must_null (MayNulls.min_elt may_nulls_set) then
        (MustNulls.singleton min_must_null, MayNulls.singleton min_must_null, Idx.of_int !Cil.kindOfSizeOf (Z.add min_must_null Z.one))
      (* else return empty must_nulls_set and keep every index up to smallest index of must_nulls_set included in may_nulls_set *)
      else
        (MustNulls.empty (), MayNulls.filter (Z.geq min_must_null) may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.add min_must_null Z.one))

  let to_n_string (must_nulls_set, may_nulls_set, size) n =
    let rec add_indexes i max may_nulls_set =
      if Z.geq i max then
        may_nulls_set
      else
        add_indexes (Z.add i Z.one) max (MayNulls.add i may_nulls_set) in
    let update_must_indexes min_must_null must_nulls_set =
      if Z.equal min_must_null Z.zero then
        MustNulls.bot ()
      else
        (* if strlen < n, every byte starting from min_must_null is surely also transformed to null *)
        add_indexes min_must_null (Z.of_int n) (MustNulls.filter (Z.gt (Z.of_int n)) must_nulls_set) in
    let update_may_indexes min_may_null may_nulls_set =
      if Z.equal min_may_null Z.zero then
        MayNulls.top ()
      else
        (* if strlen < n, every byte starting from may_must_null may be transformed to null *)
        add_indexes min_may_null (Z.of_int n) (MayNulls.filter (Z.gt (Z.of_int n)) may_nulls_set) in
    let warn_no_null min_null =
      if Z.geq min_null (Z.of_int n) then
        M.warn "Resulting string might not be null-terminated because src doesn't contain a null byte in the first n bytes" in

    if n < 0 then
      (MustNulls.top (), MayNulls.top (), Idx.top_of !Cil.kindOfSizeOf)
    else
      let check_n = match Idx.minimal size, Idx.maximal size with
      | Some min, Some max ->
        if Z.gt (Z.of_int n) max then
          M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Array size is smaller than n bytes; can cause a buffer overflow"
        else if Z.gt (Z.of_int n) min then
          M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Array size might be smaller than n bytes; can cause a buffer overflow"
      | Some min, None ->
        if Z.gt (Z.of_int n) min then
          M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Array size might be smaller than n bytes; can cause a buffer overflow"
      | None, Some max ->
        if Z.gt (Z.of_int n) max then
          M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Array size is smaller than n bytes; can cause a buffer overflow"
      | None, None -> () in
      check_n;
      (* if definitely no null byte in array, i.e. must_nulls_set = may_nulls_set = empty set *)
      if MustNulls.is_empty must_nulls_set && MayNulls.is_empty may_nulls_set then
        match Idx.minimal size with
        (* ... there *may* be null bytes from minimal size to n - 1 if minimal size < n *)
        | Some min when Z.geq min Z.zero -> (must_nulls_set, add_indexes min (Z.of_int n) may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))
        | _ -> (must_nulls_set, may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))
      (* if only must_nulls_set empty, remove indexes >= n and add all indexes from min_may_null to n - 1 to may_nulls_set; 
       * warn if resulting array may not contain null byte *)
      else if MustNulls.is_empty must_nulls_set then
        let min_may_null = MayNulls.min_elt may_nulls_set in
        warn_no_null min_may_null;
        (must_nulls_set, update_may_indexes min_may_null may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))
      else
        let min_must_null = MustNulls.min_elt must_nulls_set in
        let min_may_null = MayNulls.min_elt may_nulls_set in
        warn_no_null min_may_null;
        (* if smallest index in sets coincides, remove indexes >= n and add all indexes from min_null to n - 1 to both sets; 
         * warn if resulting array may not contain null byte *)
        if Z.equal min_must_null min_may_null then
          (update_must_indexes min_must_null must_nulls_set, update_may_indexes min_may_null may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))
        (* else return empty must_nulls_set, remove indexes >= n and add all indexes from min_may_null to n - 1 to may_nulls_set; 
         * warn if resulting array may not contain null byte *)
        else
          (MustNulls.empty (), update_may_indexes min_may_null may_nulls_set, Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))

  let to_string_length (must_nulls_set, may_nulls_set, size) =
    (* if must_nulls_set and min_nulls_set empty, definitely no null byte in array => return interval [size, inf) *)
    if MustNulls.is_empty must_nulls_set && MayNulls.is_empty may_nulls_set then
      match Idx.minimal size with
      | Some min -> Idx.starting !Cil.kindOfSizeOf min
      | None -> Idx.starting !Cil.kindOfSizeOf Z.zero
    (* if only must_nulls_set empty, no guarantee that null ever encountered in array => return interval [minimal may null, inf) *)
    else if MustNulls.is_empty must_nulls_set then
      Idx.starting !Cil.kindOfSizeOf (MayNulls.min_elt may_nulls_set)
    (* else return interval [minimal may null, minimal must null] *)
    else
      Idx.of_interval !Cil.kindOfSizeOf (MustNulls.min_elt must_nulls_set, MayNulls.min_elt may_nulls_set)
      
  let string_copy (must_nulls_set1, may_nulls_set1, size1) ar2 = function
    (* strcpy *)
    | None ->
      let must_nulls_set2, may_nulls_set2, size2 = to_string ar2 in
      let strlen2 = to_string_length ar2 in
      (* filter out indexes before strlen(src) from dest sets and after strlen(src) from src sets and build union, keep size of dest *)
      begin match Idx.minimal size1, Idx.maximal size1, Idx.minimal strlen2, Idx.maximal strlen2 with
        | Some min1, Some max1, Some min2, Some max2 ->
          let warn =
            if Z.leq max1 min2 then 
              M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src is greater than the allocated size for dest" 
            else if Z.leq min1 max2 then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.union (MustNulls.filter (Z.geq max2) must_nulls_set1) (MustNulls.filter (Z.leq min1) must_nulls_set2) in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.gt min2) may_nulls_set1) (MayNulls.filter (Z.leq max1) may_nulls_set2) in
          (must_nulls_set_result, may_nulls_set_result, size1)
        | Some min1, None, Some min2, Some max2 ->
          let warn =
            if Z.leq min1 max2 then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.union (MustNulls.filter (Z.geq max2) must_nulls_set1) (MustNulls.filter (Z.leq min1) must_nulls_set2) in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.gt min2) may_nulls_set1) may_nulls_set2 in
          (must_nulls_set_result, may_nulls_set_result, size1)
        | Some min1, Some max1, Some min2, None ->
          let warn =
            if Z.leq max1 min2 then 
              M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src is greater than the allocated size for dest" 
            else if Z.leq min1 min2 then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.filter (Z.leq min1) must_nulls_set2 in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.gt min2) may_nulls_set1) (MayNulls.filter (Z.leq max1) may_nulls_set2) in
          (must_nulls_set_result, may_nulls_set_result, size1)
        | Some min1, None, Some min2, None ->
          let warn =
            if Z.leq min1 min2 then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.filter (Z.leq min1) must_nulls_set2 in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.gt min2) may_nulls_set1) may_nulls_set2 in
          (must_nulls_set_result, may_nulls_set_result, size1)
        (* any other case shouldn't happen as minimal index is always >= 0 *)
        | _ -> (MustNulls.top (), MayNulls.top (), size1)
      end
    (* strncpy => strlen(src) is precise number *)
    | Some n ->
      let must_nulls_set2, may_nulls_set2, _ = to_n_string ar2 n in
      (* filter out indexes before strlen(src) from dest sets and after strlen(src) from src sets and build union, keep size of dest *)
      begin match Idx.minimal size1, Idx.maximal size1 with
        | Some min1, Some max1 ->
          let warn =
            if Z.lt max1 (Z.of_int n) then
              M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src is greater than the allocated size for dest" 
            else if Z.lt min1 (Z.of_int n) then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.union (MustNulls.filter (Z.geq (Z.of_int n)) must_nulls_set1) (MustNulls.filter (Z.leq min1) must_nulls_set2) in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.geq (Z.of_int n)) may_nulls_set1) (MayNulls.filter (Z.leq max1) may_nulls_set2) in
          (must_nulls_set_result, may_nulls_set_result, size1)
        | Some min1, None ->
          let warn =
            if Z.lt min1 (Z.of_int n) then
              M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of string src may be greater than the allocated size for dest" in
          warn;
          let must_nulls_set_result = MustNulls.union (MustNulls.filter (Z.geq (Z.of_int n)) must_nulls_set1) (MustNulls.filter (Z.leq min1) must_nulls_set2) in
          let may_nulls_set_result = MayNulls.union (MayNulls.filter (Z.geq (Z.of_int n)) may_nulls_set1) may_nulls_set2 in
          (must_nulls_set_result, may_nulls_set_result, size1)
        (* any other case shouldn't happen as minimal index is always >= 0 *)
        | _ -> (MustNulls.top (), MayNulls.top (), size1)
      end

  let string_concat (must_nulls_set1, may_nulls_set1, size1) (must_nulls_set2, may_nulls_set2, size2) n =
    let update_sets min1 max1 max1_exists minlen1 maxlen1 maxlen1_exists minlen2 maxlen2 maxlen2_exists must_nulls_set2' may_nulls_set2' = 
      (* track any potential buffer overflow and issue warning if needed *)
      let warn =
        if max1_exists && ((maxlen1_exists && maxlen2_exists && Z.leq max1 (Z.add maxlen1 maxlen2))
          || (maxlen1_exists && Z.leq max1 (Z.add maxlen1 minlen2)) || (maxlen2_exists && Z.leq max1 (Z.add minlen1 maxlen2))
          || Z.leq max1 (Z.add minlen1 minlen2)) then
          M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of the concatenation of the strings in src and dest is greater than the allocated size for dest"
        else if (maxlen1_exists && maxlen2_exists && Z.leq min1 (Z.add maxlen1 maxlen2)) || (maxlen1_exists && Z.leq min1 (Z.add maxlen1 minlen2))
          || (maxlen2_exists && Z.leq min1 (Z.add minlen1 maxlen2)) || Z.leq min1 (Z.add minlen1 minlen2) then
          M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "The length of the conctenation of the strings in src and dest may be greater than the allocated size for dest" in
      warn;
      (* if any must_nulls_set empty, result must_nulls_set also empty; 
       * for all i1, i2 in may_nulls_set1, may_nulls_set2: add i1 + i2 if it is <= strlen(dest) + strlen(src) to new may_nulls_set
       * and keep indexes > strlen(dest) + strlen(src) of may_nulls_set *)
      if MustNulls.is_empty must_nulls_set1 || MustNulls.is_empty must_nulls_set2' then
        let may_nulls_set_result =
          MayNulls.filter (Z.geq (Z.add minlen1 minlen2)) may_nulls_set1
          |> MayNulls.elements
          |> BatList.cartesian_product (MayNulls.elements may_nulls_set2')
          |> List.map (fun (i1, i2) -> Z.add i1 i2)
          |> MayNulls.of_list
          |> MayNulls.union (MayNulls.filter (Z.lt (Z.add minlen1 minlen2)) may_nulls_set1)
          |> MayNulls.filter (fun x -> if max1_exists then Z.gt max1 x else true) in
        (MustNulls.top (), may_nulls_set_result, size1)
      (* if minimal must null = minimal may null in ar1 and ar2, add them and keep indexes > strlen(dest) + strlen(src) of ar1 *)
      else if Z.equal (MustNulls.min_elt must_nulls_set1) (MayNulls.min_elt may_nulls_set1) then
        let min_i1 = MustNulls.min_elt must_nulls_set1 in
        let min_i2 = MustNulls.min_elt must_nulls_set2' in
        let min_i = Z.add min_i1 min_i2 in
        let must_nulls_set_result = 
          MustNulls.filter (fun x -> if maxlen1_exists && maxlen2_exists then Z.lt (Z.add maxlen1 maxlen2) x else false) must_nulls_set1
          |> MustNulls.add min_i
          |> MustNulls.filter (Z.gt min1) in
        let may_nulls_set_result = 
          MayNulls.filter (Z.lt (Z.add minlen1 minlen2)) may_nulls_set1
          |> MayNulls.add min_i
          |> MayNulls.filter (fun x -> if max1_exists then Z.gt max1 x else true) in
        (must_nulls_set_result, may_nulls_set_result, size1)
      (* else only add all may nulls <= strlen(dest) + strlen(src) *)
      else
        let min_i2 = MustNulls.min_elt must_nulls_set2' in
        let must_nulls_set_result = MustNulls.filter (fun x -> if maxlen1_exists && maxlen2_exists then Z.lt (Z.add maxlen1 maxlen2) x else false) must_nulls_set1 in
        let may_nulls_set_result = 
          MayNulls.filter (Z.geq (Z.add minlen1 minlen2)) may_nulls_set1
          |> MayNulls.map (Z.add min_i2)
          |> MayNulls.union (MayNulls.filter (Z.lt (Z.add minlen1 minlen2)) may_nulls_set1)
          |> MayNulls.filter (fun x -> if max1_exists then Z.gt max1 x else true) in
        (must_nulls_set_result, may_nulls_set_result, size1) in
    let compute_concat must_nulls_set2' may_nulls_set2' =
      let strlen1 = to_string_length (must_nulls_set1, may_nulls_set1, size1) in
      let strlen2 = to_string_length (must_nulls_set2', may_nulls_set2', size2) in
      begin match Idx.minimal size1, Idx.maximal size1, Idx.minimal strlen1, Idx.maximal strlen1, Idx.minimal strlen2, Idx.maximal strlen2 with
        | Some min1, Some max1, Some minlen1, Some maxlen1, Some minlen2, Some maxlen2 ->
          update_sets min1 max1 true minlen1 maxlen1 true minlen2 maxlen2 true must_nulls_set2' may_nulls_set2'
        (* no upper bound for length of concatenation *)
        | Some min1, Some max1, Some minlen1, None, Some minlen2, Some _
        | Some min1, Some max1, Some minlen1, Some _, Some minlen2, None
        
        | Some min1, Some max1, Some minlen1, None, Some minlen2, None -> 
          update_sets min1 max1 true minlen1 Z.zero false minlen2 Z.zero false must_nulls_set2' may_nulls_set2'
        (* no upper bound for size of dest *)
        | Some min1, None, Some minlen1, Some maxlen1, Some minlen2, Some maxlen2 ->
          update_sets min1 Z.zero false minlen1 maxlen1 true minlen2 maxlen2 true must_nulls_set2' may_nulls_set2'
        (* no upper bound for size of dest and length of concatenation *)
        | Some min1, None, Some minlen1, None, Some minlen2, Some _
        | Some min1, None, Some minlen1, Some _, Some minlen2, None
        | Some min1, None, Some minlen1, None, Some minlen2, None ->
          update_sets min1 Z.zero false minlen1 Z.zero false minlen2 Z.zero false must_nulls_set2' may_nulls_set2'
        (* any other case shouldn't happen as minimal index is always >= 0 *)
        | _ -> (MustNulls.top (), MayNulls.top (), size1)
      end in

    match n with
    (* strcat *)
    | None ->
      let must_nulls_set2', may_nulls_set2', _ = to_string (must_nulls_set2, may_nulls_set2, size2) in
      compute_concat must_nulls_set2' may_nulls_set2'
    (* strncat *)
    | Some num -> 
      (* take at most n bytes from src; if no null byte among them, add null byte at index n *)
      let must_nulls_set2', may_nulls_set2' =
        let must_nulls_set2, may_nulls_set2, _ = to_string (must_nulls_set2, may_nulls_set2, size2) in
        if not (MayNulls.exists (Z.gt (Z.of_int num)) may_nulls_set2) then
          (MustNulls.singleton (Z.of_int num), MayNulls.singleton (Z.of_int num))
        else if not (MustNulls.exists (Z.gt (Z.of_int num)) must_nulls_set2) then
          (MustNulls.empty (), MayNulls.add (Z.of_int num) (MayNulls.filter (Z.leq (Z.of_int num)) may_nulls_set2))
        else
          (MustNulls.filter (Z.leq (Z.of_int num)) must_nulls_set2, MayNulls.filter (Z.leq (Z.of_int num)) may_nulls_set2) in
      compute_concat must_nulls_set2' may_nulls_set2'

  let substring_extraction haystack (must_nulls_set_needle, may_nulls_set_needle, size_needle) =
    (* if needle is empty string, i.e. certain null byte at index 0, return haystack as string *)
    if MustNulls.mem Z.zero must_nulls_set_needle then
      to_string haystack
    else
      let haystack_len = to_string_length haystack in
      let needle_len = to_string_length (must_nulls_set_needle, may_nulls_set_needle, size_needle) in      
      match Idx.maximal haystack_len, Idx.minimal needle_len with
      | Some haystack_max, Some needle_min ->
        (* if strlen(haystack) < strlen(needle), needle can never be substring of haystack => return null pointer -- TODO: how to do that? *)
        if Z.lt haystack_max needle_min then
          (MustNulls.top (), MayNulls.top (), Idx.of_int !Cil.kindOfSizeOf Z.zero)
        else
          (MustNulls.top (), MayNulls.top (), Idx.top_of !Cil.kindOfSizeOf)
      | _ -> (MustNulls.top (), MayNulls.top (), Idx.top_of !Cil.kindOfSizeOf)

  let string_comparison (must_nulls_set1, may_nulls_set1, _) (must_nulls_set2, may_nulls_set2, _) = function
    (* strcmp *)
    | None ->
      (* if s1 = s2 = empty string, i.e. certain null byte at index 0, return 0 *)
      if MustNulls.mem Z.zero must_nulls_set1 && (MustNulls.mem Z.zero must_nulls_set2) then
        Idx.of_int IInt Z.zero
      (* if only s1 = empty string, return negative integer *)
      else if MustNulls.mem Z.zero must_nulls_set1 && not (MustNulls.mem Z.zero must_nulls_set2) then
        Idx.ending IInt Z.minus_one
      (* if only s2 = empty string, return positive integer *)
      else if MustNulls.mem Z.zero must_nulls_set2 then
        Idx.starting IInt Z.one
      else 
        (* if first null bytes are certain and have different indexes, return integer <> 0 *)
        (try if Z.equal (MustNulls.min_elt must_nulls_set1) (MayNulls.min_elt may_nulls_set1) 
            && Z.equal (MustNulls.min_elt must_nulls_set2) (MayNulls.min_elt may_nulls_set2)
            && not (Z.equal (MustNulls.min_elt must_nulls_set1) (MustNulls.min_elt must_nulls_set2)) then
          Idx.join (Idx.ending IInt Z.minus_one) (Idx.starting IInt Z.one)
        else
          Idx.top_of IInt
        with Not_found -> Idx.top_of IInt)
    (* strncmp *)
    | Some num ->
      (* if s1 = empty and s2 = empty string or n = 0, return 0 *)
      if MustNulls.mem Z.zero must_nulls_set1 && ((MustNulls.mem Z.zero must_nulls_set2) || Z.equal Z.zero (Z.of_int num)) then
        Idx.of_int IInt Z.zero
      (* if only s1 = empty string, return negative integer *)
      else if MustNulls.mem Z.zero must_nulls_set1 && not (MustNulls.mem Z.zero must_nulls_set2) then
        Idx.ending IInt Z.minus_one
      (* if only s2 = empty string, return positive integer *)
      else if MustNulls.mem Z.zero must_nulls_set2 then
        Idx.starting IInt Z.one
      else 
        (* if first null bytes are certain, have different indexes and are before index n for s2, return integer <> 0 *)
        (try if Z.equal (MustNulls.min_elt must_nulls_set1) (MayNulls.min_elt may_nulls_set1) 
            && Z.equal (MustNulls.min_elt must_nulls_set2) (MayNulls.min_elt may_nulls_set2)
            && Z.lt (MustNulls.min_elt must_nulls_set2) (Z.of_int num)
            && not (Z.equal (MustNulls.min_elt must_nulls_set1) (MustNulls.min_elt must_nulls_set2)) then
          Idx.join (Idx.ending IInt Z.minus_one) (Idx.starting IInt Z.one)
        else
          Idx.top_of IInt
        with Not_found -> Idx.top_of IInt)

  let update_length _ x = x

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t
end

module AttributeConfiguredArrayDomain(Val: LatticeWithNull) (Idx:IntDomain.Z):S with type value = Val.t and type idx = Idx.t =
struct
  module P = PartitionedWithLength(Val)(Idx)
  module T = TrivialWithLength(Val)(Idx)
  module U = UnrollWithLength(Val)(Idx)
  module N = NullByte(Val)(Idx)

  type idx = Idx.t
  type value = Val.t

  module K = struct
    let msg = "AttributeConfiguredArrayDomain received a value where not exactly one component is set"
    let name = "AttributeConfiguredArrayDomain"
  end

  let to_t = function
    | (Some p, None, None) -> (Some p, None)
    | (None, Some t, None) -> (None, Some (Some t, None))
    | (None, None, Some u) -> (None, Some (None, Some u))
    | _ -> failwith "AttributeConfiguredArrayDomain received a value where not exactly one component is set"

  module I = struct include LatticeFlagHelper (T) (U) (K) let name () = "" end
  include LatticeFlagHelper (P) (I) (K)
  (* include Lattice.Prod (LatticeFlagHelper (P) (I) (K)) (N) *)

  let domain_of_t = function
    | (Some p, None) -> PartitionedDomain
    | (None, Some (Some t, None)) -> TrivialDomain
    | (None, Some (None, Some u)) -> UnrolledDomain
    | _ -> failwith "Array of invalid domain"

  let binop' opp opt opu = binop opp (I.binop opt opu)
  let unop' opp opt opu = unop opp (I.unop opt opu)
  let binop_to_t' opp opt opu = binop_to_t opp (I.binop_to_t opt opu)
  let unop_to_t' opp opt opu = unop_to_t opp (I.unop_to_t opt opu)

  (* Simply call appropriate function for component that is not None *)
  let get ?(checkBounds=true) a x (e,i) = unop' (fun x ->
      if e = None then
        let e' = BatOption.map (fun x -> Cil.kintegerCilint (Cilfacade.ptrdiff_ikind ()) x) (Idx.to_int i) in
        P.get ~checkBounds a x (e', i)
      else
        P.get ~checkBounds a x (e, i)
    ) (fun x -> T.get ~checkBounds a x (e,i)) (fun x -> U.get ~checkBounds a x (e,i)) x
  let set (ask:VDQ.t) x i a = unop_to_t' (fun x -> P.set ask x i a) (fun x -> T.set ask x i a) (fun x -> U.set ask x i a) x
  let length = unop' P.length T.length U.length
  let map f = unop_to_t' (P.map f) (T.map f) (U.map f)
  let fold_left f s = unop' (P.fold_left f s) (T.fold_left f s) (U.fold_left f s)

  let move_if_affected ?(replace_with_const=false) (ask:VDQ.t) x v f = unop_to_t' (fun x -> P.move_if_affected ~replace_with_const:replace_with_const ask x v f) (fun x -> T.move_if_affected ~replace_with_const:replace_with_const ask x v f) (fun x -> U.move_if_affected ~replace_with_const:replace_with_const ask x v f) x
  let get_vars_in_e = unop' P.get_vars_in_e T.get_vars_in_e U.get_vars_in_e
  let smart_join f g = binop_to_t' (P.smart_join f g) (T.smart_join f g) (U.smart_join f g)
  let smart_widen f g =  binop_to_t' (P.smart_widen f g) (T.smart_widen f g) (U.smart_widen f g)
  let smart_leq f g = binop' (P.smart_leq f g) (T.smart_leq f g) (U.smart_leq f g)
  let update_length newl x = unop_to_t' (P.update_length newl) (T.update_length newl) (U.update_length newl) x
  let name () = "AttributeConfiguredArrayDomain"

  let bot () = to_t @@ match get_domain ~varAttr:[] ~typAttr:[] with
    | PartitionedDomain -> (Some (P.bot ()), None, None)
    | TrivialDomain -> (None, Some (T.bot ()), None)
    | UnrolledDomain ->  (None, None, Some (U.bot ()))

  let top () = to_t @@ match get_domain ~varAttr:[] ~typAttr:[] with
    | PartitionedDomain -> (Some (P.top ()), None, None)
    | TrivialDomain -> (None, Some (T.top ()), None)
    | UnrolledDomain -> (None, None, Some (U.top ()))

  let make ?(varAttr=[]) ?(typAttr=[]) i v = to_t @@  match get_domain ~varAttr ~typAttr with
    | PartitionedDomain -> (Some (P.make i v), None, None)
    | TrivialDomain -> (None, Some (T.make i v), None)
    | UnrolledDomain -> (None, None, Some (U.make i v))

  (* convert to another domain *)
  let index_as_expression i = (Some (Cil.integer i), Idx.of_int IInt (BI.of_int i))

  let partitioned_of_trivial ask t = P.make (Option.value (T.length t) ~default:(Idx.top ())) (T.get ~checkBounds:false ask t (index_as_expression 0))

  let partitioned_of_unroll ask u =
    (* We end with a partition at "ana.base.arrays.unrolling-factor", which keeps the most information. Maybe first element is more commonly useful? *)
    let rest = (U.get ~checkBounds:false ask u (index_as_expression (factor ()))) in
    let p = P.make (Option.value (U.length u) ~default:(Idx.top ())) rest in
    let get_i i = (i, P.get ~checkBounds:false ask p (index_as_expression i)) in
    let set_i p (i,v) =  P.set ask p (index_as_expression i) v in
    List.fold_left set_i p @@ List.init (factor ()) get_i

  let trivial_of_partitioned ask p =
    let element = (P.get ~checkBounds:false ask p (None, Idx.top ()))
    in T.make (Option.value (P.length p) ~default:(Idx.top ())) element

  let trivial_of_unroll ask u =
    let get_i i = U.get ~checkBounds:false ask u (index_as_expression i) in
    let element = List.fold_left Val.join (get_i (factor ())) @@ List.init (factor ()) get_i in (*join all single elements and the element at  *)
    T.make (Option.value (U.length u) ~default:(Idx.top ())) element

  let unroll_of_trivial ask t = U.make (Option.value (T.length t) ~default:(Idx.top ())) (T.get ~checkBounds:false ask t (index_as_expression 0))

  let unroll_of_partitioned ask p =
    let unrolledValues = List.init (factor ()) (fun i ->(i, P.get ~checkBounds:false ask p (index_as_expression i))) in
    (* This could be more precise if we were able to compare this with the partition index, but we can not access it here *)
    let rest = (P.get ~checkBounds:false ask p (None, Idx.top ())) in
    let u = U.make (Option.value (P.length p) ~default:(Idx.top ())) (Val.bot ()) in
    let set_i u (i,v) =  U.set ask u (index_as_expression i) v in
    set_i (List.fold_left set_i u unrolledValues) (factor (), rest)

  let project ?(varAttr=[]) ?(typAttr=[]) ask (t:t) =
    match get_domain ~varAttr ~typAttr, t with
    | PartitionedDomain, (Some x, None) -> to_t @@ (Some x, None, None)
    | PartitionedDomain, (None, Some (Some x, None)) -> to_t @@ (Some (partitioned_of_trivial ask x), None, None)
    | PartitionedDomain, (None, Some (None, Some x)) -> to_t @@ (Some (partitioned_of_unroll ask x), None, None)
    | TrivialDomain, (Some x, None) -> to_t @@ (None, Some (trivial_of_partitioned ask x), None)
    | TrivialDomain, (None, Some (Some x, None)) -> to_t @@ (None, Some x, None)
    | TrivialDomain, (None, Some (None, Some x)) -> to_t @@ (None, Some (trivial_of_unroll ask x), None)
    | UnrolledDomain, (Some x, None) -> to_t @@ (None, None, Some (unroll_of_partitioned ask x) )
    | UnrolledDomain, (None, Some (Some x, None)) -> to_t @@ (None, None, Some (unroll_of_trivial ask x) )
    | UnrolledDomain, (None, Some (None, Some x)) -> to_t @@ (None, None, Some x)
    | _ ->  failwith "AttributeConfiguredArrayDomain received a value where not exactly one component is set"
end
