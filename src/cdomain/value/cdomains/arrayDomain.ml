open GoblintCil
open Pretty
open GobConfig
open FlagHelper

module M = Messages
module A = Array
module BI = IntOps.BigIntOps
module VDQ = ValueDomainQueries

type domain = TrivialDomain | PartitionedDomain | UnrolledDomain

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

module type S0 =
sig
  include Lattice.S
  type idx
  type value

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
  val invariant: value_invariant:(offset:Cil.offset -> lval:Cil.lval -> value -> Invariant.t) -> offset:Cil.offset -> lval:Cil.lval -> t -> Invariant.t
end

module type S =
sig
  include S0

  val domain_of_t: t -> domain
  val get: ?checkBounds:bool -> VDQ.t -> t -> Basetype.CilExp.t option * idx -> value
end

module type Str =
sig
  include S0

  type ret = Null | NotNull | Maybe
  type substr = IsNotSubstr | IsSubstrAtIndex0 | IsMaybeSubstr

  val get: VDQ.t -> t -> Basetype.CilExp.t option * idx -> ret

  val to_null_byte_domain: string -> t
  val to_string_length: t -> idx
  val string_copy: t -> t -> int option -> t
  val string_concat: t -> t -> int option -> t
  val substring_extraction: t -> t -> substr
  val string_comparison: t -> t -> int option -> idx
end

module type StrWithDomain =
sig
  include Str
  include S with type t := t and type idx := idx
end

module type LatticeWithInvalidate =
sig
  include Lattice.S
  val invalidate_abstract_value: t -> t
end

module type LatticeWithSmartOps =
sig
  include LatticeWithInvalidate
  val smart_join: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> t
  val smart_widen: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> t
  val smart_leq: (Cil.exp -> BI.t option) -> (Cil.exp -> BI.t option) -> t -> t -> bool
end

module type Null =
sig
  type t
  type retnull = Null | NotNull | Maybe

  val null: unit -> t
  val is_null: t -> retnull

  val get_ikind: t -> Cil.ikind option
  val zero_of_ikind: Cil.ikind -> t
  val not_zero_of_ikind: Cil.ikind -> t
end

module type LatticeWithNull =
sig
  include LatticeWithSmartOps
  include Null with type t := t
end

module Trivial (Val: LatticeWithInvalidate) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
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
  let set (ask: VDQ.t) a (ie, i) v =
    match ie with
    | Some ie when CilType.Exp.equal ie Offset.Index.Exp.all ->
      v
    | _ ->
      join a v
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

  let invariant ~value_invariant ~offset ~lval x =
    match offset with
    (* invariants for all indices *)
    | NoOffset when get_bool "witness.invariant.goblint" ->
      let i_lval = Cil.addOffsetLval (Index (Offset.Index.Exp.all, NoOffset)) lval in
      value_invariant ~offset ~lval:i_lval x
    | NoOffset ->
      Invariant.none
    (* invariant for one index *)
    | Index (i, offset) ->
      value_invariant ~offset ~lval x
    (* invariant for one field *)
    | Field (f, offset) ->
      Invariant.none
end

let factor () =
  match get_int "ana.base.arrays.unrolling-factor" with
  | 0 -> failwith "ArrayDomain: ana.base.arrays.unrolling-factor needs to be set when using the unroll domain"
  | x -> x

module Unroll (Val: LatticeWithInvalidate) (Idx:IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
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
  let set ask (xl, xr) (ie, i) v =
    match ie with
    | Some ie when CilType.Exp.equal ie Offset.Index.Exp.all ->
      (* TODO: Doesn't seem to work for unassume because unrolled elements are top-initialized, not bot-initialized. *)
      (BatList.make (factor ()) v, v)
    | _ ->
      set ask (xl, xr) (ie, i) v

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

  let invariant ~value_invariant ~offset ~lval ((xl, xr) as x) =
    match offset with
    (* invariants for all indices *)
    | NoOffset ->
      let i_all =
        if Val.is_bot xr then
          Invariant.top ()
        else if get_bool "witness.invariant.goblint" then (
          let i_lval = Cil.addOffsetLval (Index (Offset.Index.Exp.all, NoOffset)) lval in
          value_invariant ~offset ~lval:i_lval (join_of_all_parts x)
        )
        else
          Invariant.top ()
      in
      BatList.fold_lefti (fun acc i x ->
          let i_lval = Cil.addOffsetLval (Index (Cil.integer i, NoOffset)) lval in
          let i = value_invariant ~offset ~lval:i_lval x in
          Invariant.(acc && i)
        ) i_all xl
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none (* TODO: look up *)
    (* invariant for one field *)
    | Field (f, offset) ->
      Invariant.none
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
    match i with
    | Some ie when CilType.Exp.equal ie Offset.Index.Exp.all ->
      (* TODO: Doesn't seem to work for unassume. *)
      Joint a
    | Some i when CilType.Exp.equal i Offset.Index.Exp.any ->
      (assert !AnalysisState.global_initialization; (* just joining with xm here assumes that all values will be set, which is guaranteed during inits *)
       (* the join is needed here! see e.g 30/04 *)
       let o = match x with Partitioned (_, (_, xm, _)) -> xm | Joint v -> v in
       let r =  Val.join o a in
       Joint r)
    | _ ->
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

  let invariant ~value_invariant ~offset ~lval x =
    match offset with
    (* invariants for all indices *)
    | NoOffset when get_bool "witness.invariant.goblint" ->
      let i_lval = Cil.addOffsetLval (Index (Offset.Index.Exp.all, NoOffset)) lval in
      value_invariant ~offset ~lval:i_lval (join_of_all_parts x)
    | NoOffset ->
      Invariant.none
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none (* TODO: look up *)
    (* invariant for one field *)
    | Field (f, offset) ->
      Invariant.none
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
      AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
      M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Must access array past end"
    | Some true, None ->
      AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "May access array past end"
    | Some false, Some true ->
      AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
      M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "Must access array before start"
    | None, Some true ->
      AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "May access array before start"
    | _ ->
      AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
      M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.unknown "May access array out of bounds"
  else ()


module TrivialWithLength (Val: LatticeWithInvalidate) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
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

  let invariant ~value_invariant ~offset ~lval (x, _) =
    Base.invariant ~value_invariant ~offset ~lval x

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

  let invariant ~value_invariant ~offset ~lval (x, _) =
    Base.invariant ~value_invariant ~offset ~lval x

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y

  let to_yojson (x, y) = `Assoc [ (Base.name (), Base.to_yojson x); ("length", Idx.to_yojson y) ]
end

module UnrollWithLength (Val: LatticeWithInvalidate) (Idx: IntDomain.Z): S with type value = Val.t and type idx = Idx.t =
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

  let invariant ~value_invariant ~offset ~lval (x, _) =
    Base.invariant ~value_invariant ~offset ~lval x

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y

  let to_yojson (x, y) = `Assoc [ (Base.name (), Base.to_yojson x); ("length", Idx.to_yojson y) ]
end

module NullByte (Val: LatticeWithNull) (Idx: IntDomain.Z): Str with type value = Val.t and type idx = Idx.t =
struct
  module MustSet = NullByteSet.MustSet
  module MaySet = NullByteSet.MaySet
  module Nulls = NullByteSet.MustMaySet

  let (<.) = Z.lt
  let (<=.) = Z.leq
  let (>.) = Z.gt
  let (>=.) = Z.geq
  let (=.) = Z.equal
  let (+.) = Z.add

  (* (Must Null Set, May Null Set, Array Size) *)
  include Lattice.Prod (Nulls) (Idx)

  let name () = "arrays containing null bytes"
  type idx = Idx.t
  type value = Val.t

  type ret = Null | NotNull | Maybe

  type substr = IsNotSubstr | IsSubstrAtIndex0 | IsMaybeSubstr

  module ArrayOobMessage = M.Category.Behavior.Undefined.ArrayOutOfBounds
  let warn_past_end = M.error ~category:ArrayOobMessage.past_end

  let min_nat_of_idx i = Z.max Z.zero (BatOption.default Z.zero (Idx.minimal i))

  let get (ask: VDQ.t) (nulls, size) (e, i) =
    let min_i = min_nat_of_idx i in
    let max_i = Idx.maximal i in
    let min_size = min_nat_of_idx size in

    match max_i, Idx.maximal size with
    (* if there is no maximum value in index interval *)
    | None, _ when not (Nulls.exists Possibly ((<=.) min_i) nulls) ->
      (* ... return NotNull if no i >= min_i in may_nulls_set *)
      NotNull
    | None, _ ->
      (* ... else return Top *)
      Maybe
    (* if there is no maximum size *)
    | Some max_i, None when max_i >=. Z.zero ->
      (* ... and maximum value in index interval < minimal size, return Null if all numbers in index interval are in must_nulls_set *)
      if max_i <. min_size && Nulls.interval_mem Definitely (min_i,max_i) nulls then
        Null
        (* ... return NotNull if no number in index interval is in may_nulls_set *)
      else if not (Nulls.exists Possibly (fun x -> x >=. min_i && x <=. max_i) nulls) then
        NotNull
      else
        Maybe
    | Some max_i, Some max_size when max_i >=. Z.zero ->
      (* if maximum value in index interval < minimal size, return Null if all numbers in index interval are in must_nulls_set *)
      if max_i <. min_size && Nulls.interval_mem Definitely (min_i, max_i) nulls then
        Null
        (* if maximum value in index interval < maximal size, return NotNull if no number in index interval is in may_nulls_set *)
      else if max_i <. max_size && not (Nulls.exists Possibly (fun x -> x >=. min_i && x <=. max_i) nulls) then
        NotNull
      else
        Maybe
    (* if maximum number in interval is invalid, i.e. negative, return Top of value *)
    | _ -> Maybe

  let set (ask: VDQ.t) (nulls, size) (e, i) v =
    let min_size = min_nat_of_idx size in
    let min_i = min_nat_of_idx i in
    let max_i = Idx.maximal i in

    let set_exact_nulls i =
      match Idx.maximal size with
      (* if size has no upper limit *)
      | None ->
        (match Val.is_null v with
         | NotNull ->
           Nulls.remove (if Nulls.is_full_set Possibly nulls then Possibly else Definitely) i nulls min_size
         (* ... and value <> null, remove i from must_nulls_set and also from may_nulls_set if not top *)
         | Null ->
           Nulls.add (if i <. min_size then Definitely else Possibly) i nulls
         (* i < minimal size and value = null, add i to must_nulls_set and may_nulls_set *)
         (* i >= minimal size and value = null, add i only to may_nulls_set *)
         | Maybe ->
           let removed = Nulls.remove Possibly i nulls min_size in
           Nulls.add Possibly i removed)
      | Some max_size ->
        (match Val.is_null v with
         | NotNull ->
           Nulls.remove Definitely i nulls min_size
         (* if value <> null, remove i from must_nulls_set and may_nulls_set *)
         | Null when i <. min_size ->
           Nulls.add Definitely i nulls
         | Null when i <. max_size ->
           Nulls.add Possibly i nulls
         | Maybe when i <. max_size ->
           let removed = Nulls.remove Possibly i nulls min_size in
           Nulls.add Possibly i removed
         | _ -> nulls
        )
    in

    let set_interval min_i max_i =
      (* Update max_i so it is capped at the maximum size *)
      let max_i = BatOption.map_default (fun x -> Z.min max_i @@ Z.pred x) max_i (Idx.maximal size) in
      match Val.is_null v with
      | NotNull -> Nulls.remove_interval Possibly (min_i, max_i) min_size nulls
      | Null -> Nulls.add_interval ~maxfull:(Idx.maximal size) Possibly (min_i, max_i) nulls
      | Maybe ->
        let nulls = Nulls.add_interval ~maxfull:(Idx.maximal size) Possibly (min_i, max_i) nulls in
        Nulls.remove_interval Possibly (min_i, max_i) min_size nulls
    in

    (* warn if index is (potentially) out of bounds *)
    array_oob_check (module Idx) (Nulls.get_set Possibly, size) (e, i);
    let nulls = match max_i with
      (* if no maximum number in index interval *)
      | None ->
        (* ..., value = null *)
        (if Val.is_null v = Null && Idx.maximal size = None then
           match Idx.maximal size with
           (* ... and there is no maximal size, modify may_nulls_set to top *)
           | None ->  Nulls.add_all Possibly nulls
           (* ... and there is a maximal size, add all i from minimal index to maximal size to may_nulls_set *)
           | Some max_size -> Nulls.add_interval Possibly (min_i, Z.pred max_size) nulls
           (* ... and value <> null, only keep indexes < minimal index in must_nulls_set *)
         else if Val.is_null v = NotNull then
           Nulls.filter_musts (Z.gt min_i) min_size nulls
           (*..., value unknown *)
         else
           match Idx.minimal size, Idx.maximal size with
           (* ... and size unknown, modify both sets to top *)
           | None, None -> Nulls.top ()
           (* ... and only minimal size known, remove all indexes < minimal size from must_nulls_set and modify may_nulls_set to top *)
           | Some min_size, None ->
             let nulls = Nulls.add_all Possibly nulls in
             Nulls.filter_musts (Z.gt min_size) min_size nulls
           (* ... and only maximal size known, modify must_nulls_set to top and add all i from minimal index to maximal size to may_nulls_set *)
           | None, Some max_size ->
             let nulls = Nulls.remove_all Possibly nulls in
             Nulls.add_interval Possibly (min_i, Z.pred max_size) nulls
           (* ... and size is known, remove all indexes < minimal size from must_nulls_set and add all i from minimal index to maximal size to may_nulls_set *)
           | Some min_size, Some max_size ->
             let nulls = Nulls.filter_musts (Z.gt min_size) min_size nulls in
             Nulls.add_interval Possibly (min_i, Z.pred max_size) nulls
        )
      | Some max_i when max_i >=. Z.zero ->
        if min_i =. max_i then
          set_exact_nulls min_i
        else
          set_interval min_i max_i
      (* if maximum number in interval is invalid, i.e. negative, return tuple unmodified *)
      | _ -> nulls
    in
    (nulls, size)


  let make ?(varAttr=[]) ?(typAttr=[]) i v =
    let min_i, max_i = match Idx.minimal i, Idx.maximal i with
      | Some min_i, Some max_i ->
        if min_i <. Z.zero && max_i <. Z.zero then
          (M.error ~category:ArrayOobMessage.before_start "Tries to create an array of negative size";
           Z.zero, Some Z.zero)
        else if min_i <. Z.zero then
          (M.warn ~category:ArrayOobMessage.before_start "May try to create an array of negative size";
           Z.zero, Some max_i)
        else
          min_i, Some max_i
      | None, Some max_i ->
        if max_i <. Z.zero then
          (M.error ~category:ArrayOobMessage.before_start "Tries to create an array of negative size";
           Z.zero, Some Z.zero)
        else
          Z.zero, Some max_i
      | Some min_i, None ->
        if min_i <. Z.zero then
          (M.warn ~category:ArrayOobMessage.before_start "May try to create an array of negative size";
           Z.zero, None)
        else
          min_i, None
      | None, None -> Z.zero, None
    in
    let size = BatOption.map_default (fun max -> Idx.of_interval ILong (min_i, max)) (Idx.starting ILong min_i) max_i in
    match Val.is_null v with
    | Null -> (Nulls.make_all_must (), size)
    | NotNull -> (Nulls.empty (), size)
    | Maybe -> (Nulls.top (), size)


  let length (_, size) = Some size

  let move_if_affected ?(replace_with_const=false) _ x _ _ = x

  let get_vars_in_e _ = []

  let map f (nulls, size) =
    (* if f(null) = null, all values in must_nulls_set still are surely null;
     * assume top for may_nulls_set as checking effect of f for every possible value is unfeasbile *)
    match Val.is_null (f (Val.null ())) with
    | Null -> (Nulls.add_all Possibly nulls, size)
    | _ -> (Nulls.top (), size) (* else also return top for must_nulls_set *)

  let fold_left f acc _ = f acc (Val.top ())

  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq

  (* string functions *)

  let to_null_byte_domain s =
    let last_null = Z.of_int (String.length s) in
    let rec build_set i set =
      if (Z.of_int i) >=. last_null then
        Nulls.Set.add last_null set
      else
        match String.index_from_opt s i '\x00' with
        | Some i -> build_set (i + 1) (Nulls.Set.add (Z.of_int i) set)
        | None -> Nulls.Set.add last_null set in
    let set = build_set 0 (Nulls.Set.empty ()) in
    (Nulls.precise_set set, Idx.of_int ILong (Z.succ last_null))

  (** Returns an abstract value with at most one null byte marking the end of the string *)
  let to_string ((nulls, size) as x:t):t =
    (* if must_nulls_set and min_nulls_set empty, definitely no null byte in array => warn about certain buffer overflow and return tuple unchanged *)
    if Nulls.is_empty Definitely nulls then
      (warn_past_end "Array access past end: buffer overflow"; x)
      (* if only must_nulls_set empty, no certainty about array containing null byte => warn about potential buffer overflow and return tuple unchanged *)
    else if Nulls.is_empty Possibly nulls then
      (warn_past_end "May access array past end: potential buffer overflow"; x)
    else
      let min_must_null = Nulls.min_elem Definitely nulls in
      let new_size = Idx.of_int ILong (Z.succ min_must_null) in
      let min_may_null = Nulls.min_elem Possibly nulls in
      (* if smallest index in sets coincides, only this null byte is kept in both sets *)
      let nulls =
        if min_must_null =. min_may_null then
          Nulls.precise_singleton min_must_null
          (* else return empty must_nulls_set and keep every index up to smallest index of must_nulls_set included in may_nulls_set *)
        else
          match Idx.maximal size with
          | Some max_size ->
            let nulls' = Nulls.remove_all Possibly nulls in
            Nulls.filter ~max_size (Z.leq min_must_null) nulls'
          | None when not (Nulls.may_can_benefit_from_filter nulls) ->
            Nulls.add_interval Possibly (Z.zero, min_must_null) (Nulls.empty ())
          | None ->
            let nulls' = Nulls.remove_all Possibly nulls in
            Nulls.filter (Z.leq min_must_null) nulls'
      in
      (nulls, new_size)

  (** [to_n_string index_set n] returns an abstract value with a potential null byte
    * marking the end of the string and if needed followed by further null bytes to obtain
    * an n bytes string. *)
  let to_n_string (nulls, size) n:t =
    if n < 0 then
      (Nulls.top (), Idx.top_of ILong)
    else
      let n = Z.of_int n in
      let warn_no_null min_must_null min_may_null =
        if Z.geq min_may_null n then
          M.warn "Resulting string might not be null-terminated because src doesn't contain a null byte in the first n bytes"
        else
          (match min_must_null with
           | Some min_must_null when not (min_must_null >=. n || min_must_null >. min_may_null) -> ()
           | _ ->
             M.warn "Resulting string might not be null-terminated because src might not contain a null byte in the first n bytes"
          )
      in
      (match Idx.minimal size, Idx.maximal size with
       | Some min_size, Some max_size ->
         if n >. max_size then
           warn_past_end "Array size is smaller than n bytes; can cause a buffer overflow"
         else if n >. min_size then
           warn_past_end "Array size might be smaller than n bytes; can cause a buffer overflow"
       | Some min_size, None ->
         if n >. min_size then
           warn_past_end "Array size might be smaller than n bytes; can cause a buffer overflow"
       | None, Some max_size ->
         if n >. max_size then
           warn_past_end "Array size is smaller than n bytes; can cause a buffer overflow"
       | None, None -> ());
      let nulls =
        (* if definitely no null byte in array, i.e. must_nulls_set = may_nulls_set = empty set *)
        if Nulls.is_empty Definitely nulls then
          (warn_past_end
             "Resulting string might not be null-terminated because src doesn't contain a null byte";
           match Idx.maximal size with
           (* ... there *may* be null bytes from maximal size to n - 1 if maximal size < n (i.e. past end) *)
           | Some max_size when Z.geq max_size Z.zero -> Nulls.add_interval Possibly (max_size, Z.pred n) nulls
           | _ -> nulls)
          (* if only must_nulls_set empty, remove indexes >= n from may_nulls_set and add all indexes from minimal may null index to n - 1;
           * warn as in any case, resulting array not guaranteed to contain null byte *)
        else if Nulls.is_empty Possibly nulls then
          let min_may_null = Nulls.min_elem Possibly nulls in
          warn_no_null None min_may_null;
          if min_may_null =. Z.zero then
            Nulls.add_all Possibly nulls
          else
            let nulls = Nulls.add_interval Possibly (min_may_null, Z.pred n) nulls in
            Nulls.filter (fun x -> x <. n) nulls
        else
          let min_must_null = Nulls.min_elem Definitely nulls in
          let min_may_null = Nulls.min_elem Possibly nulls in
          (* warn if resulting array may not contain null byte *)
          warn_no_null (Some min_must_null) min_may_null;
          (* if min_must_null = min_may_null, remove indexes >= n and add all indexes from minimal must/may null to n - 1 in the sets *)
          if min_must_null =. min_may_null then
            if min_must_null =. Z.zero then
              Nulls.full_set ()
            else
              let nulls = Nulls.add_interval Definitely (min_must_null, Z.pred n) nulls in
              let nulls = Nulls.add_interval Possibly (min_may_null, Z.pred n) nulls in
              Nulls.filter (fun x -> x <. n) nulls
          else if min_may_null =. Z.zero then
            Nulls.top ()
          else
            let nulls = Nulls.remove_all Possibly nulls in
            let nulls = Nulls.add_interval Possibly (min_may_null, Z.pred n) nulls in
            Nulls.filter (fun x -> x <. n) nulls
      in
      (nulls,  Idx.of_int ILong n)

  let to_string_length (nulls, size) =
    (* if must_nulls_set and min_nulls_set empty, definitely no null byte in array => return interval [size, inf) and warn *)
    if Nulls.is_empty Definitely nulls then
      (warn_past_end "Array doesn't contain a null byte: buffer overflow";
       Idx.starting !Cil.kindOfSizeOf (BatOption.default Z.zero (Idx.minimal size))
      )
      (* if only must_nulls_set empty, no guarantee that null ever encountered in array => return interval [minimal may null, inf) and *)
    else if Nulls.is_empty Possibly nulls then
      (warn_past_end "Array might not contain a null byte: potential buffer overflow";
       Idx.starting !Cil.kindOfSizeOf (Nulls.min_elem Possibly nulls))
      (* else return interval [minimal may null, minimal must null] *)
    else
      Idx.of_interval !Cil.kindOfSizeOf (Nulls.min_elem Possibly nulls, Nulls.min_elem Definitely nulls)

  let string_copy (dstnulls, dstsize) ((srcnulls, srcsize) as src) n =
    let must_nulls_set1, may_nulls_set1 = dstnulls in
    (* filter out indexes before strlen(src) from dest sets and after strlen(src) from src sets and build union, keep size of dest *)
    let update_sets (truncatednulls, truncatedsize) len2 =
      let must_nulls_set2',may_nulls_set2' = truncatednulls in
      match Idx.minimal dstsize, Idx.maximal dstsize, Idx.minimal len2, Idx.maximal len2 with
      | Some min_dstsize, Some max_dstsize, Some min_srclen, Some max_srclen ->
        (if max_dstsize <. min_srclen then
           warn_past_end "The length of string src is greater than the allocated size for dest"
         else if min_dstsize <. max_srclen then
           warn_past_end "The length of string src may be greater than the allocated size for dest");
        let must_nulls_set_result =
          let min_size2 = BatOption.default Z.zero (Idx.minimal truncatedsize) in
          (* get must nulls from src string < minimal size of dest *)
          MustSet.filter ~min_size:min_size2 (Z.gt min_dstsize) must_nulls_set2'
          (* and keep indexes of dest >= maximal strlen of src *)
          |> MustSet.union (MustSet.filter ~min_size:min_dstsize (Z.leq max_srclen) must_nulls_set1) in
        let may_nulls_set_result =
          let max_size2 = BatOption.default max_dstsize (Idx.maximal truncatedsize) in
          (* get may nulls from src string < maximal size of dest *)
          MaySet.filter ~max_size:max_size2 (Z.gt max_dstsize) may_nulls_set2'
          (* and keep indexes of dest >= minimal strlen of src *)
          |> MaySet.union (MaySet.filter ~max_size:max_dstsize (Z.leq min_srclen) may_nulls_set1) in
        ((must_nulls_set_result, may_nulls_set_result), dstsize)


      | Some min_size1, None, Some min_len2, Some max_len2 ->
        (if min_size1 <. max_len2 then
           warn_past_end "The length of string src may be greater than the allocated size for dest");
        let must_nulls_set_result =
          let min_size2 = BatOption.default Z.zero (Idx.minimal truncatedsize) in
          MustSet.filter ~min_size: min_size2 (Z.gt min_size1) must_nulls_set2'
          |> MustSet.union (MustSet.filter ~min_size:min_size1 (Z.leq max_len2) must_nulls_set1) in
        let may_nulls_set_result =
          (* get all may nulls from src string as no maximal size of dest *)
          may_nulls_set2'
          |> MaySet.union (MaySet.filter ~max_size:(Z.succ min_len2) (Z.leq min_len2) may_nulls_set1)  in
        ((must_nulls_set_result, may_nulls_set_result), dstsize)
      | Some min_size1, Some max_size1, Some min_len2, None ->
        (if max_size1 <. min_len2 then
           warn_past_end "The length of string src is greater than the allocated size for dest"
         else if min_size1 <. min_len2 then
           warn_past_end"The length of string src may be greater than the allocated size for dest");
        (* do not keep any index of dest as no maximal strlen of src *)
        let must_nulls_set_result =
          let min_size2 = BatOption.default Z.zero (Idx.minimal truncatedsize) in
          MustSet.filter ~min_size:min_size2 (Z.gt min_size1) must_nulls_set2' in
        let may_nulls_set_result =
          let max_size2 = BatOption.default max_size1 (Idx.maximal truncatedsize) in
          MaySet.filter ~max_size:max_size2 (Z.gt max_size1) may_nulls_set2'
          |> MaySet.union (MaySet.filter ~max_size:max_size1 (Z.leq min_len2) may_nulls_set1) in
        ((must_nulls_set_result, may_nulls_set_result), dstsize)
      | Some min_size1, None, Some min_len2, None ->
        (if min_size1 <. min_len2 then
           warn_past_end "The length of string src may be greater than the allocated size for dest");
        (* do not keep any index of dest as no maximal strlen of src *)
        let min_size2 = BatOption.default Z.zero (Idx.minimal truncatedsize) in
        let truncatednulls = Nulls.remove_interval Possibly (Z.zero, min_size1) min_size2 truncatednulls in
        let filtered_dst = Nulls.filter ~max_size:(Z.succ min_len2) (Z.leq min_len2) dstnulls in
        (* get all may nulls from src string as no maximal size of dest *)
        (Nulls.union_mays truncatednulls filtered_dst, dstsize)
      (* any other case shouldn't happen as minimal index is always >= 0 *)
      | _ -> (Nulls.top (), dstsize) in

    (* warn if size of dest is (potentially) smaller than size of src and the latter (potentially) has no null byte at index < size of dest *)
    let sizes_warning srcsize =
      (match Idx.minimal dstsize, Idx.maximal dstsize, Idx.minimal srcsize, Idx.maximal srcsize with
       | Some min_dstsize, _, Some min_srcsize, _ when min_dstsize <. min_srcsize ->
         if not (Nulls.exists Possibly (Z.gt min_dstsize) srcnulls) then
           warn_past_end "src doesn't contain a null byte at an index smaller than the size of dest"
         else if not (Nulls.exists Definitely (Z.gt min_dstsize) srcnulls) then
           warn_past_end "src may not contain a null byte at an index smaller than the size of dest"
       | Some min_dstsize, _, _, Some max_srcsize when min_dstsize <. max_srcsize ->
         if not (Nulls.exists Possibly (Z.gt min_dstsize) srcnulls) then
           warn_past_end "src doesn't contain a null byte at an index smaller than the size of dest"
         else if not (Nulls.exists Definitely (Z.gt min_dstsize) srcnulls) then
           warn_past_end "src may not contain a null byte at an index smaller than the size of dest"
       | Some min_dstsize, _, _, None ->
         if not (Nulls.exists Definitely (Z.gt min_dstsize) srcnulls) then
           warn_past_end "src may not contain a null byte at an index smaller than the size of dest"
       | _, Some mac_dstsize, _, Some max_srcsize when mac_dstsize <. max_srcsize ->
         if not (Nulls.exists Definitely (Z.gt mac_dstsize) srcnulls) then
           warn_past_end "src may not contain a null byte at an index smaller than the size of dest"
       |_, Some max_dstsize, _, None ->
         if not (Nulls.exists Definitely (Z.gt max_dstsize) srcnulls) then
           warn_past_end "src may not contain a null byte at an index smaller than the size of dest"
       | _ -> ()) in

    match n with
    (* strcpy *)
    | None ->
      sizes_warning srcsize;
      let truncated = to_string src in
      update_sets truncated (to_string_length src)
    (* strncpy = exactly n bytes from src are copied to dest *)
    | Some n when n >= 0 ->
      sizes_warning (Idx.of_int ILong (Z.of_int n));
      let truncated = to_n_string src n in
      update_sets truncated (Idx.of_int !Cil.kindOfSizeOf (Z.of_int n))
    | _ -> (Nulls.top (), dstsize)

  let string_concat (nulls1, size1) (nulls2, size2) n =
    let update_sets min_size1 max_size1 minlen1 maxlen1 minlen2 (maxlen2: Z.t option) nulls2' =
      (* track any potential buffer overflow and issue warning if needed *)
      (if GobOption.exists (fun x -> x <=. (minlen1 +. minlen2)) max_size1 then
         warn_past_end
           "The length of the concatenation of the strings in src and dest is greater than the allocated size for dest"
       else
         (match maxlen1, maxlen2 with
          | Some maxlen1, Some maxlen2 when min_size1 >. (maxlen1 +. maxlen2) -> ()
          | _ -> warn_past_end
                   "The length of the concatenation of the strings in src and dest may be greater than the allocated size for dest")
      );
      (* if any must_nulls_set empty, result must_nulls_set also empty;
       * for all i1, i2 in may_nulls_set1, may_nulls_set2: add i1 + i2 if it is <= strlen(dest) + strlen(src) to new may_nulls_set
       * and keep indexes > minimal strlen(dest) + strlen(src) of may_nulls_set *)
      if Nulls.is_empty Possibly nulls1 || Nulls.is_empty Possibly nulls2 then
        match max_size1 with
        | Some max_size1 ->
          let nulls1_no_must = Nulls.remove_all Possibly nulls1 in
          let pred = match maxlen1, maxlen2 with
            | Some maxlen1, Some maxlen2 -> (fun x -> x <=. (maxlen1 +. maxlen2))
            | _ -> (fun _ -> true)
          in
          let r =
            nulls1_no_must
            (* filter ensures we have the concete representation *)
            |> Nulls.filter ~max_size:max_size1 pred
            |> Nulls.elements ~max_size:max_size1 Possibly
            |> BatList.cartesian_product (Nulls.elements ~max_size:max_size1 Possibly nulls2')
            |> List.map (fun (i1, i2) -> i1 +. i2)
            |> (fun x -> Nulls.add_list Possibly x (Nulls.filter ~max_size:max_size1 (Z.lt (minlen1 +. minlen2)) nulls1_no_must))
            |> Nulls.filter (Z.gt max_size1)
          in
          (r, size1)
        | None when Nulls.may_can_benefit_from_filter nulls1 && Nulls.may_can_benefit_from_filter nulls2 ->
          (match maxlen1, maxlen2 with
           | Some maxlen1, Some maxlen2->
             let nulls1_no_must = Nulls.remove_all Possibly nulls1 in
             let r =
               nulls1_no_must
               (* filter ensures we have the concete representation *)
               |> Nulls.filter (fun x -> x <=. (maxlen1 +. maxlen2))
               |> Nulls.elements Possibly
               |> BatList.cartesian_product (Nulls.elements Possibly nulls2')
               |> List.map (fun (i1, i2) -> i1 +. i2)
               |> (fun x -> Nulls.add_list Possibly x (Nulls.filter (Z.lt (minlen1 +. minlen2)) nulls1_no_must))
             in
             (r, size1)
           | _ -> (Nulls.top (), size1))
        |  _ -> (Nulls.top (), size1)
        (* if minimal must null = minimal may null in ar1 and ar2, add them together and keep indexes > strlen(dest) + strlen(src) of ar1 *)
      else if Nulls.min_elem_precise nulls1 && Nulls.min_elem_precise nulls2' then
        let min_i1 = Nulls.min_elem Definitely nulls1 in
        let min_i2 = Nulls.min_elem Definitely nulls2' in
        let min_i = min_i1 +. min_i2 in
        let (must_nulls_set1, may_nulls_set1) = nulls1 in
        let must_nulls_set_result =
          MustSet.filter ~min_size:min_size1 (Z.lt min_i) must_nulls_set1
          |> MustSet.add min_i
          |> MustSet.M.filter (Z.gt min_size1) in
        let may_nulls_set_result =
          match max_size1 with
          | Some max_size1 ->
            MaySet.filter ~max_size:max_size1 (Z.lt min_i) may_nulls_set1
            |> MaySet.add min_i
            |> MaySet.M.filter (fun x -> max_size1 >. x)
          | _ -> MaySet.top ()
        in
        ((must_nulls_set_result, may_nulls_set_result), size1)
        (* else only add all may nulls together <= strlen(dest) + strlen(src) *)
      else
        let min_i2 = Nulls.min_elem Definitely nulls2' in
        let (must_nulls_set1, may_nulls_set1) = nulls1 in
        let (must_nulls_set2', may_nulls_set2') = nulls2' in
        let may_nulls_set2'_until_min_i2 =
          match Idx.maximal size2 with
          | Some max_size2 -> MaySet.filter ~max_size:max_size2 (Z.geq min_i2) may_nulls_set2'
          | None -> MaySet.filter ~max_size:(Z.succ min_i2) (Z.geq min_i2) may_nulls_set2' in
        let must_nulls_set_result =
          let pred = match maxlen1, maxlen2 with
            | Some maxlen1, Some maxlen2 -> (fun x -> (maxlen1 +. maxlen2) <. x)
            | _ -> (fun _ -> false)
          in
          MustSet.filter ~min_size:min_size1 pred must_nulls_set1
        in
        let may_nulls_set_result =
          let pred = match maxlen1, maxlen2 with
            | Some maxlen1, Some maxlen2 -> (fun x -> x <=. (maxlen1 +. maxlen2))
            | _ -> (fun _ -> true)
          in
          match max_size1 with
          | Some max_size1 ->
            MaySet.filter ~max_size:max_size1 pred may_nulls_set1
            |> MaySet.elements
            |> BatList.cartesian_product (MaySet.elements may_nulls_set2'_until_min_i2)
            |> List.map (fun (i1, i2) -> i1 +. i2)
            |> MaySet.of_list
            |> MaySet.union (MaySet.filter ~max_size:max_size1 (Z.lt (minlen1 +. minlen2)) may_nulls_set1)
            |> MaySet.M.filter (fun x -> max_size1 >. x)
          | None when not (MaySet.is_top may_nulls_set1) ->
            MaySet.M.filter pred may_nulls_set1
            |> MaySet.elements
            |> BatList.cartesian_product (MaySet.elements may_nulls_set2'_until_min_i2)
            |> List.map (fun (i1, i2) -> i1 +. i2)
            |> MaySet.of_list
            |> MaySet.union (MaySet.M.filter (Z.lt (minlen1 +. minlen2)) may_nulls_set1)
          | _ ->
            MaySet.top () in
        ((must_nulls_set_result, may_nulls_set_result), size1) in

    let compute_concat nulls2' =
      let strlen1 = to_string_length (nulls1, size1) in
      let strlen2 = to_string_length (nulls2', size2) in
      match Idx.minimal size1, Idx.minimal strlen1, Idx.minimal strlen2 with
      | Some min_size1, Some minlen1, Some minlen2 ->
        begin
          let f = update_sets min_size1 (Idx.maximal size1) minlen1 in
          match Idx.maximal strlen1, Idx.maximal strlen2 with
          | (Some _ as maxlen1), (Some _ as maxlen2) -> f maxlen1 minlen2 maxlen2 nulls2'
          | _ -> f None minlen2 None nulls2'
        end
      (* any other case shouldn't happen as minimal index is always >= 0 *)
      | _ -> (Nulls.top (), size1) in

    match n with
    (* strcat *)
    | None ->
      let nulls2', _ = to_string (nulls2, size2) in
      compute_concat nulls2'
    (* strncat *)
    | Some n when n >= 0 ->
      let n = Z.of_int n in
      (* take at most n bytes from src; if no null byte among them, add null byte at index n *)
      let nulls2' =
        let (nulls2, size2) = to_string (nulls2, size2) in
        if not (Nulls.exists Possibly (Z.gt n) nulls2) then
          Nulls.precise_singleton n
        else if not (Nulls.exists Definitely (Z.gt n) nulls2) then
          let max_size = BatOption.default (Z.succ n) (Idx.maximal size2) in
          let nulls2 = Nulls.remove_all Possibly nulls2 in
          let nulls2 = Nulls.filter ~max_size (Z.geq n) nulls2 in
          Nulls.add Possibly n nulls2
        else
          let min_size = BatOption.default Z.zero (Idx.minimal size2) in
          let max_size = BatOption.default n (Idx.maximal size2) in
          Nulls.filter ~max_size ~min_size (Z.gt n) nulls2
      in
      compute_concat nulls2'
    | _ -> (Nulls.top (), size1)

  let substring_extraction haystack ((nulls_needle, size_needle) as needle) =
    (* if needle is empty string, i.e. certain null byte at index 0, return value of strstr is pointer to haystack *)
    if Nulls.mem Definitely Z.zero nulls_needle then
      IsSubstrAtIndex0
    else
      let haystack_len = to_string_length haystack in
      let needle_len = to_string_length needle in
      match Idx.maximal haystack_len, Idx.minimal needle_len with
      | Some haystack_max, Some needle_min when haystack_max <. needle_min ->
        (* if strlen(haystack) < strlen(needle), needle can never be substring of haystack => return None *)
        IsNotSubstr
      | _ -> IsMaybeSubstr

  let string_comparison (nulls1, size1) (nulls2, size2) n =
    let cmp n =
      (* if s1 = s2 = empty string, i.e. certain null byte at index 0, or n = 0, return 0 *)
      if (Nulls.mem Definitely Z.zero nulls1 && Nulls.mem Definitely Z.zero nulls2) || (BatOption.map_default (Z.equal Z.zero) false n) then
        Idx.of_int IInt Z.zero
        (* if only s1 = empty string, return negative integer *)
      else if Nulls.mem Definitely Z.zero nulls1 && not (Nulls.mem Possibly Z.zero nulls2) then
        Idx.ending IInt Z.minus_one
        (* if only s2 = empty string, return positive integer *)
      else if Nulls.mem Definitely Z.zero nulls2 then
        Idx.starting IInt Z.one
      else
        try
          let min_must1 = Nulls.min_elem Definitely nulls1 in
          let min_must2 = Nulls.min_elem Definitely nulls2 in
          if not (min_must1 =. min_must2)
          && min_must1 =.(Nulls.min_elem Possibly nulls1)
          && min_must2 =. (Nulls.min_elem Possibly nulls2)
          && (BatOption.map_default (fun x -> min_must1 <. x || min_must2 <. x) true n)
          then
            (* if first null bytes are certain, have different indexes and are before index n if n present, return integer <> 0 *)
            Idx.of_excl_list IInt [Z.zero]
          else
            Idx.top_of IInt
        with Not_found -> Idx.top_of IInt
    in

    match n with
    (* strcmp *)
    | None ->
      (* track any potential buffer overflow and issue warning if needed *)
      let warn_missing_nulls nulls name =
        if Nulls.is_empty Definitely nulls then
          warn_past_end "Array of string %s doesn't contain a null byte: buffer overflow" name
        else if Nulls.is_empty Possibly nulls then
          warn_past_end "Array of string %s might not contain a null byte: potential buffer overflow" name
      in
      warn_missing_nulls nulls1 "1";
      warn_missing_nulls nulls2 "2";
      (* compute abstract value for result of strcmp *)
      cmp None
    (* strncmp *)
    | Some n when n >= 0 ->
      let n = Z.of_int n in
      let warn_size size name =
        let min = min_nat_of_idx size in
        match Idx.maximal size with
        | Some max when n >. max ->
          warn_past_end "The size of the array of string %s is smaller than n bytes" name
        | Some max when n >. min ->
          warn_past_end "The size of the array of string %s might be smaller than n bytes" name
        | None when n >. min ->
          warn_past_end "The size of the array of string %s might be smaller than n bytes" name
        | _ -> ()
      in
      warn_size size1 "1";
      warn_size size2 "2";
      (* compute abstract value for result of strncmp *)
      cmp (Some n)
    | _ -> Idx.top_of IInt

  let update_length new_size (nulls, size) = (nulls, new_size)

  let project ?(varAttr=[]) ?(typAttr=[]) _ t = t

  let invariant ~value_invariant ~offset ~lval x = Invariant.none
end

module AttributeConfiguredArrayDomain(Val: LatticeWithSmartOps) (Idx:IntDomain.Z):S with type value = Val.t and type idx = Idx.t =
struct
  module P = PartitionedWithLength(Val)(Idx)
  module T = TrivialWithLength(Val)(Idx)
  module U = UnrollWithLength(Val)(Idx)

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
  let name () = "FlagHelperAttributeConfiguredArrayDomain"

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

  let invariant ~value_invariant ~offset ~lval =
    unop'
      (P.invariant ~value_invariant ~offset ~lval)
      (T.invariant ~value_invariant ~offset ~lval)
      (U.invariant ~value_invariant ~offset ~lval)
end

module AttributeConfiguredAndNullByteArrayDomain (Val: LatticeWithNull) (Idx: IntDomain.Z): StrWithDomain with type value = Val.t and type idx = Idx.t =
struct
  module A = AttributeConfiguredArrayDomain (Val) (Idx)
  module N = NullByte (Val) (Idx)

  include Lattice.Prod (A) (N)

  let name () = "AttributeConfiguredAndNullByteArrayDomain"
  type idx = Idx.t
  type value = Val.t

  type ret = Null | NotNull | Maybe
  type substr = N.substr = IsNotSubstr | IsSubstrAtIndex0 | IsMaybeSubstr

  let domain_of_t (t_f, _) = A.domain_of_t t_f

  let get ?(checkBounds=true) (ask: VDQ.t) (t_f, t_n) i =
    let f_get = A.get ~checkBounds ask t_f i in
    if get_bool "ana.base.arrays.nullbytes" then
      let n_get = N.get ask t_n i in
      match Val.get_ikind f_get, n_get with
      | Some ik, Null -> Val.meet f_get (Val.zero_of_ikind ik)
      | Some ik, NotNull -> Val.meet f_get (Val.not_zero_of_ikind ik)
      | _ -> f_get
    else
      f_get

  let construct a n =
    if get_bool "ana.base.arrays.nullbytes" then
      (a, n ())
    else
      (a, N.top ())

  let set (ask:VDQ.t) (t_f, t_n) i v = construct (A.set ask t_f i v) (fun () -> N.set ask t_n i v)
  let make ?(varAttr=[]) ?(typAttr=[]) i v = construct (A.make ~varAttr ~typAttr i v) (fun () -> N.make ~varAttr ~typAttr i v)
  let map f (t_f, t_n) = construct (A.map f t_f) (fun () -> N.map f t_n)
  let update_length newl (t_f, t_n) = construct (A.update_length newl t_f) (fun () -> N.update_length newl t_n)

  let smart_binop op_a op_n x y (t_f1, t_n1) (t_f2, t_n2) = construct (op_a x y t_f1 t_f2) (fun () -> op_n x y t_n1 t_n2)

  let smart_join = smart_binop A.smart_join N.smart_join
  let smart_widen = smart_binop A.smart_widen N.smart_widen

  let string_op op (t_f1, t_n1) (_, t_n2) n = construct (A.map Val.invalidate_abstract_value t_f1) (fun () -> op t_n1 t_n2 n)
  let string_copy = string_op N.string_copy
  let string_concat = string_op N.string_concat

  let extract op default (_, t_n1) (_, t_n2) n =
    if get_bool "ana.base.arrays.nullbytes" then
      op t_n1 t_n2 n
    else
      (* Hidden behind unit, as constructing defaults may happen to early otherwise *)
      (* e.g. for Idx.top_of IInt *)
      default ()

  let substring_extraction x y = extract (fun x y _  -> N.substring_extraction x y) (fun () -> IsMaybeSubstr) x y None
  let string_comparison = extract N.string_comparison (fun () -> Idx.top_of IInt)

  let length (t_f, t_n) =
    if get_bool "ana.base.arrays.nullbytes" then
      N.length t_n
    else
      A.length t_f
  let move_if_affected ?(replace_with_const=false) (ask:VDQ.t) (t_f, t_n) v f = (A.move_if_affected ~replace_with_const ask t_f v f, N.move_if_affected ~replace_with_const ask t_n v f)
  let get_vars_in_e (t_f, _) = A.get_vars_in_e t_f
  let fold_left f acc (t_f, _) = A.fold_left f acc t_f

  let smart_leq x y (t_f1, t_n1) (t_f2, t_n2) =
    if get_bool "ana.base.arrays.nullbytes" then
      A.smart_leq x y t_f1 t_f2 && N.smart_leq x y t_n1 t_n2
    else
      A.smart_leq x y t_f1 t_f2

  let to_null_byte_domain s =
    if get_bool "ana.base.arrays.nullbytes" then
      (A.make (Idx.top_of ILong) (Val.meet (Val.not_zero_of_ikind IChar) (Val.zero_of_ikind IChar)), N.to_null_byte_domain s)
    else
      (A.top (), N.top ())
  let to_string_length (_, t_n) =
    if get_bool "ana.base.arrays.nullbytes" then
      N.to_string_length t_n
    else
      Idx.top_of !Cil.kindOfSizeOf

  let project ?(varAttr=[]) ?(typAttr=[]) ask (t_f, t_n) = (A.project ~varAttr ~typAttr ask t_f, N.project ~varAttr ~typAttr ask t_n)
  let invariant ~value_invariant ~offset ~lval (t_f, _) = A.invariant ~value_invariant ~offset ~lval t_f
end
