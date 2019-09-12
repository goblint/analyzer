open Pretty
open Cil
open GobConfig

module M = Messages
module A = Array
module GU = Goblintutil
module Q = Queries

module type S =
sig
  include Lattice.S
  type idx
  type value

  val get: Q.ask -> t ->  ExpDomain.t * idx -> value
  val set: ?length:(int64 option) -> Q.ask -> t -> ExpDomain.t * idx -> value -> t
  val make: int -> value -> t
  val length: t -> int option

  val move_if_affected: ?length:(int64 option) -> Q.ask -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
  val get_vars_in_e: t -> Cil.varinfo list
  val map: (value -> value) -> t -> t
  val fold_left: ('a -> value -> 'a) -> 'a -> t -> 'a
  val fold_left2: ('a -> value -> value -> 'a) -> 'a -> t -> t -> 'a
  val smart_join: ?length:(int64 option) -> (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> t
  val smart_widen: ?length:(int64 option) -> (exp -> int64 option) -> (exp -> int64 option)  -> t -> t-> t
  val smart_leq: ?length:(int64 option) -> (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> bool
end

module type LatticeWithSmartOps =
sig
  include Lattice.S
  val smart_join: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> t
  val smart_widen: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> t
  val smart_leq: (Cil.exp -> int64 option) -> (Cil.exp -> int64 option) -> t -> t -> bool
end


module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  let name () = "trivial arrays"
  include Val
  type idx = Idx.t
  type value = Val.t

  let short w x = "Array: " ^ Val.short (w - 7) x
  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m
  let get (ask: Q.ask) a i = a
  let set ?(length=None) (ask: Q.ask) a i v = join a v
  let make i v = v
  let length _ = None

  let move_if_affected ?(length = None) _ x _ _ = x
  let get_vars_in_e _ = []
  let map f x = f x
  let fold_left f a x = f a x
  let fold_left2 f a x y = f a x y

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
  let smart_join ?(length=None) _ _ = join
  let smart_widen ?(length=None) _ _ = widen
  let smart_leq ?(length=None)_ _ = leq
end

module Partitioned (Val: LatticeWithSmartOps) (Idx:Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  (* Contrary to the description in Michael's master thesis, abstract values here always have the form *)
  (* (Expp, (Val, Val, Val)). Expp is top when the array is not partitioned. In these cases all three  *)
  (* values from Val are identical *)

  let name () = "partitioned arrays"
  module Expp = ExpDomain
  module Base = Lattice.Prod3 (Val) (Val) (Val)
  include Lattice.ProdSimple(Expp) (Base)
  type idx = Idx.t
  type value = Val.t

  let is_not_partitioned (e, _) =
    Expp.is_bot e || Expp.is_top e

  let join_of_all_parts (_,(xl, xm, xr)) =
    let result = Val.join (Val.join xl xm) xr
    in
    if Val.is_bot result then
      Val.top()
    else
      result

  let short w ((e,(xl, xm, xr)) as x) =
    if is_not_partitioned x then
      "Array (no part.): " ^ Val.short (w - 18) xl
    else
      "Array (part. by " ^ Expp.short (w-7) e ^ "): (" ^
        Val.short ((w - 7)/3) xl ^ " -- " ^
        Val.short ((w - 7)/3) xm ^ " -- " ^
        Val.short ((w - 7)/3) xr ^ ")"

  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m

  let get (ask:Q.ask) ((e, (xl, xm, xr)) as x) (i,_) =
    match e, i with
    | `Lifted e', `Lifted i' ->
      begin
        let isEqual = match ask (Q.MustBeEqual (e',i')) with
          | `Bool true -> true
          | _ -> false in
        if isEqual then xm
        else
          begin
            let contributionLess = match ask (Q.MayBeLess (i', e')) with        (* (may i < e) ? xl : bot *)
            | `Bool false -> Val.bot ()
            | _ -> xl in
            let contributionEqual = match ask (Q.MayBeEqual (i', e')) with      (* (may i = e) ? xm : bot *)
            | `Bool false -> Val.bot ()
            | _ -> xm in
            let contributionGreater =  match ask (Q.MayBeLess (e', i')) with    (* (may i > e) ? xr : bot *)
            | `Bool false -> Val.bot ()
            | _ -> xr in
            Val.join (Val.join contributionLess contributionEqual) contributionGreater
          end
      end
    | _ -> join_of_all_parts x

  let get_vars_in_e (e, _) =
    match e with
    | `Top
    | `Bot -> []
    | `Lifted exp -> Basetype.CilExp.get_vars exp

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
        |	UnOp(_, e , _) -> contains_array_access e
        |	BinOp(_, e1, e2, _) -> contains_array_access e1 || contains_array_access e2
        | AddrOf _
        | AddrOfLabel _
        | StartOf _ -> false
        | Lval(Mem e, o) -> offset_contains_array_access o || contains_array_access e
        | Lval(Var _, o) -> offset_contains_array_access o
    in
    match e with
    | `Top
    | `Bot -> true
    | `Lifted exp ->
      let vars = Basetype.CilExp.get_vars exp in
      List.exists (fun x -> x.vglob) vars || contains_array_access exp


  let map f (e, (xl, xm, xr)) =
    (e, (f xl, f xm, f xr))

  let fold_left f a (_, ((xl:value), (xm:value), (xr:value))) =
    f (f (f a xl) xm) xr

  let fold_left2 f a (_, ((xl:value), (xm:value), (xr:value))) (_, ((yl:value), (ym:value), (yr:value))) =
    f (f (f a xl yl) xm ym) xr yr

  let move_if_affected ?(length=None) (ask:Q.ask) ((e, (xl,xm, xr)) as x) (v:varinfo) movement_for_exp =
    let move (i:int option) =
      match i with
      | Some 0   ->
        (e, (xl, xm, xr))
      | Some 1   ->
        (e, (Val.join xl xm, xr, xr)) (* moved one to the right *)
      | Some -1  ->
        (e, (xl, xl, Val.join xm xr)) (* moved one to the left  *)
      | Some x when x > 1 ->
        (e, (Val.join (Val.join xl xm) xr, xr, xr)) (* moved more than one to the right *)
      | Some x when x < -1 ->
        (e, (xl, xl, Val.join (Val.join xl xm) xr)) (* moved more than one to the left *)
      | _ ->
        begin
          let nval = join_of_all_parts x in
          (Expp.top (), (nval, nval, nval))
        end
    in
    match e with
    | `Lifted exp ->
        let is_affected = Basetype.CilExp.occurs v exp in
        if not is_affected then
          x
        else
          (* check if one part covers the entire array, so we can drop partitioning *)
          begin
            let e_must_bigger_max_index =
              match length with
              | Some l ->
                begin
                  match ask (Q.MayBeLess (exp, Cil.kinteger64 Cil.IInt l)) with
                  | `Bool false -> true (* !(e <_{may} length) => e >=_{must} length *)
                  | _ -> false
                end
              | _ -> false
            in
            let e_must_less_zero =
              match ask (Q.MayBeLess (Cil.integer (-1), exp)) with
              | `Bool false -> true (* !(-1 <_{may} e) => e <=_{must} -1 *)
              | _ -> false
            in
            if e_must_bigger_max_index then
              (* Entire array is covered by left part, dropping partitioning. *)
              Expp.top(),(xl, xl, xl)
            else if e_must_less_zero then
              (* Entire array is covered by right value, dropping partitioning. *)
              Expp.top(),(xr, xr, xr)
            else
              (* If we can not drop partitioning, move *)
              move (movement_for_exp exp)
          end
    | _ -> x (* If the array is not partitioned, nothing to do *)

  let set ?(length=None) (ask:Q.ask) ((e, (xl, xm, xr)) as x) (i,_) a =
    begin
      let lubIfNotBot x = if Val.is_bot x then x else Val.join a x in
      if is_not_partitioned (e, (xl, xm, xr)) then
        if not_allowed_for_part i then
          let result = Val.join a (join_of_all_parts x) in
          (e, (result, result, result))
        else
          begin
            let exp_value =
              match i with
              | `Lifted i' ->
                  begin
                    match ask (Q.EvalInt i') with
                    | `Int n -> Q.ID.to_int n
                    | _ -> None
                  end
              |_ -> None in
            let e_equals_zero = BatOption.map_default (Int64.equal Int64.zero) false exp_value in
            let e_equals_maxIndex =
              match length with
              | Some l ->
                  BatOption.map_default (Int64.equal (Int64.sub l Int64.one)) false exp_value
              | _ -> false
            in
            let l = if e_equals_zero then Val.bot () else join_of_all_parts x in
            let r = if e_equals_maxIndex then Val.bot () else join_of_all_parts x in
            (i, (l, a, r))
          end
      else
        match e, i with
        | `Lifted e', `Lifted i' -> begin
            let default =
              begin
                let left =
                  match ask (Q.MayBeLess (i', e')) with     (* (may i < e) ? xl : bot *)
                  | `Bool false -> xl
                  | _ -> lubIfNotBot xl in
                let middle =
                  match ask (Q.MayBeEqual (i', e')) with    (* (may i = e) ? xm : bot *)
                  | `Bool false -> xm
                  | _ -> Val.join xm a in
                let right =
                  match ask (Q.MayBeLess (e', i')) with     (* (may i > e) ? xr : bot *)
                  | `Bool false -> xr
                  | _ -> lubIfNotBot xr in
                (e, (left, middle, right))
              end
            in
            let isEqual = match ask (Q.MustBeEqual (e',i')) with
              | `Bool true -> true
              | _ -> false
            in
            if isEqual then
              (*  e = _{must} i => update strongly *)
              (e, (xl, a, xr))
            else if Cil.isConstant e' && Cil.isConstant i' then
              match Cil.isInteger e', Cil.isInteger i' with
                | Some e'', Some i'' ->
                  if i'' = Int64.add e'' Int64.one then
                    (* If both are integer constants and they are directly adjacent, we change partitioning to maintain information *)
                    (i, (Val.join xl xm, a, xr))
                  else if e'' = Int64.add i'' Int64.one then
                    (i, (xl, a, Val.join xm xr))
                  else
                    default
                | _ ->
                  default
            else
              default
          end
        | _ ->
          (* If the expression used to write is not known, all segements except the empty ones will be affected *)
          (e, (lubIfNotBot xl, Val.join xm a, lubIfNotBot xr))
    end

  let join ((e1, (xl1,xm1,xr1)) as x1) ((e2, (xl2,xm2,xr2)) as x2) =
    let new_e = Expp.join e1 e2 in
    if Expp.is_top new_e then
      (* At least one of them was not partitioned, or e != f *)
      let join_over_all = Val.join (join_of_all_parts x1) (join_of_all_parts x2) in
      (new_e, (join_over_all, join_over_all, join_over_all))
    else
      (new_e, (Val.join xl1 xl2, Val.join xm1 xm2, Val.join xr1 xr2))

  (* leq needs not be given explictly, leq from product domain works here *)

  let make i v =
    if i = 1 then
      (`Lifted (Cil.integer 0), (Val.bot (), v, Val.bot ()))
    else if Val.is_bot v then
      (Expp.top(), (Val.top(), Val.top(), Val.top()))
    else
      (Expp.top(), (v, v, v))

  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f (e, (xl, xm, xr)) =
    let join_over_all = Val.join (Val.join xl xm) xr in
    BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml join_over_all

  let smart_op (op: Val.t -> Val.t -> Val.t) length ((e1, (xl1,xm1,xr1)) as x1) ((e2, (xl2,xm2,xr2)) as x2) x1_eval_int x2_eval_int =
    let must_be_length_minus_one v = match length with
      | Some l -> v = Some (Int64.sub l Int64.one)
      | None -> false
    in
    let must_be_zero v = v = Some Int64.zero in
    let op_over_all = op (join_of_all_parts x1) (join_of_all_parts x2) in
    match e1, e2 with
    | `Lifted e1e, `Lifted e2e when Basetype.CilExp.equal e1e e2e ->
      (e1, (op xl1 xl2, op xm1 xm2, op xr1 xr2))
    | `Lifted _, `Lifted _
    | `Top, `Top ->
      (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | `Top, `Lifted e2e ->
      if must_be_zero (x1_eval_int e2e) then
        (e2, (xl2, op xm1 xm2, op xr1 xr2))
      else if must_be_length_minus_one (x1_eval_int e2e) then
        (e2, (op xl1 xl2, op xm1 xm2, xr2))
      else
        (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | `Lifted e1e, `Top ->
      if must_be_zero (x2_eval_int e1e) then
        (e1, (xl1, op xm1 xm2, op xr1 xr2))
      else if must_be_length_minus_one (x2_eval_int e1e) then
        (e1, (op xl1 xl2, op xm1 xm2, xr1))
      else
        (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | _ ->
      failwith "ArrayDomain: Unallowed state (one of the partitioning expressions is bot)"


  let smart_join ?(length=None) x1_eval_int x2_eval_int x1 x2 =
    smart_op (Val.smart_join x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_widen ?(length=None) x1_eval_int x2_eval_int x1 x2  =
    smart_op (Val.smart_widen x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_leq ?(length=None) x1_eval_int x2_eval_int (e1, (xl1,xm1,xr1)) (e2, (xl2, xm2, xr2)) =
    let leq' = Val.smart_leq x1_eval_int x2_eval_int in
    let must_be_zero v = (v = Some Int64.zero) in
    let must_be_length_minus_one v =  match length with
      | Some l -> v = Some (Int64.sub l Int64.one)
      | None -> false
    in
    match e1, e2 with
    | `Top, `Top ->
      (* Those asserts ensure that for both arguments all segements are equal (as it should be) *)
      assert(Val.leq xl1 xm1); assert(Val.leq xm1 xr1); assert(Val.leq xl2 xm2); assert(Val.leq xm2 xr2);
      assert(Val.leq xm1 xl1); assert(Val.leq xr1 xm1); assert(Val.leq xm2 xl2); assert(Val.leq xr2 xm2);
      leq' (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2))    (* TODO: should the inner joins also be smart joins? *)
    | `Lifted _, `Top -> leq' (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2))
    | `Lifted e1e, `Lifted e2e -> Basetype.CilExp.equal e1e e2e && leq' xl1 xl2 && leq' xm1 xm2 && leq' xr1 xr2
    | `Top, `Lifted e2e ->
      if must_be_zero (x1_eval_int e2e) then
        leq' xm1 xm2 && leq' xr1 xr2
      else if must_be_length_minus_one (x1_eval_int e2e) then
        leq' xl1 xl2 && leq' xm1 xm2
      else
        false
    | _ ->
      failwith "ArrayDomain: Unallowed state (one of the partitioning expressions is bot)"
end


module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Trivial (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t
  let get (ask: Q.ask) (x ,l) i = Base.get ask x i (* TODO check if in-bounds *)
  let set ?(length=None) (ask: Q.ask) (x,l) i v = Base.set ask x i v, l
  let make l x = Base.make l x, Idx.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Idx.to_int l)

  let move_if_affected ?(length = None) _ x _ _ = x
  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
  let get_vars_in_e _ = []

  let smart_join ?(length=None) _ _ = join
  let smart_widen ?(length=None) _ _ = widen
  let smart_leq ?(length=None) _ _ = leq
end


module PartitionedWithLength (Val: LatticeWithSmartOps) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Partitioned (Val) (Idx)
  module Length = IntDomain.Flattened (* We only keep one exact value or top/bot here *)
  include Lattice.Prod (Base) (Length)
  type idx = Idx.t
  type value = Val.t
  let get ask (x,l) i = Base.get ask x i (* TODO check if in-bounds *)
  let set ?(length=None) ask (x,l) i v =
    let new_l = Length.to_int l in
    Base.set ~length:new_l ask x i v, l
  let make l x = Base.make l x, Length.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Length.to_int l)

  let move_if_affected ?(length = None) ask (x,l) v i =
    let new_l = Length.to_int l in
    (Base.move_if_affected ~length:new_l ask x v i), l

  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
  let get_vars_in_e (x, _) = Base.get_vars_in_e x

  let smart_join ?(length=None) x_eval_int y_eval_int (x,xl) (y,yl) =
    let l = Length.to_int xl in
    (Base.smart_join ~length:l x_eval_int y_eval_int x y , Length.join xl yl)

  let smart_widen ?(length=None) x_eval_int y_eval_int (x,xl) (y,yl) =
    let l = Length.to_int xl in
    (Base.smart_widen ~length:l x_eval_int y_eval_int x y , Length.join xl yl)

  let smart_leq ?(length=None) x_eval_int y_eval_int (x,xl) (y,yl)  =
    let l = Length.to_int xl in
    Base.smart_leq ~length:l x_eval_int y_eval_int x y
end

module FlagConfiguredArrayDomain(Val: LatticeWithSmartOps) (Idx:IntDomain.S):S with type value = Val.t and type idx = Idx.t =
struct
  module P = PartitionedWithLength(Val)(Idx)
  module T = TrivialWithLength(Val)(Idx)

  type idx = Idx.t
  type value = Val.t
  type t = P.t option * T.t option [@@deriving to_yojson]

  (* Helpers *)
  let binop opp opt (p1,t1) (p2,t2) = match (p1, t1),(p2, t2) with
    | (Some p1, None), (Some p2, None) -> opp p1 p2
    | (None, Some t1), (None, Some t2) -> opt t1 t2
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  let binop_to_t opp opt (p1,t1) (p2,t2)= match (p1, t1),(p2, t2) with
    | (Some p1, None), (Some p2, None) -> (Some (opp p1 p2), None)
    | (None, Some t1), (None, Some t2) -> (None, Some(opt t1 t2))
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  let unop opp opt (p,t) = match (p, t) with
    | (Some p, None) -> opp p
    | (None, Some t) -> opt t
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  let unop_to_t opp opt (p,t) = match (p, t) with
    | (Some p, None) -> (Some (opp p), None)
    | (None, Some t) -> (None, Some(opt t))
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  (* Simply call appropriate function for component that is not None *)
  let equal = binop P.equal T.equal
  let hash = unop P.hash T.hash
  let compare = binop P.compare T.compare
  let short l = unop (P.short l) (T.short l)
  let isSimple = unop P.isSimple T.isSimple
  let pretty () = unop (P.pretty ()) (T.pretty ())
  let toXML = unop P.toXML T.toXML
  let leq = binop P.leq T.leq
  let join = binop_to_t P.join T.join
  let meet = binop_to_t P.meet T.meet
  let widen = binop_to_t P.widen T.widen
  let narrow = binop_to_t P.narrow T.narrow
  let is_top = unop P.is_top T.is_top
  let is_bot = unop P.is_bot T.is_bot
  let get a x i = unop (fun x -> P.get a x i) (fun x -> T.get a x i) x
  let set ?(length=None) (ask:Q.ask) x i a = unop_to_t (fun x -> P.set ~length:length ask x i a) (fun x -> T.set ~length:length ask x i a) x
  let length = unop P.length T.length
  let get_vars_in_e = unop P.get_vars_in_e T.get_vars_in_e
  let map f = unop_to_t (P.map f) (T.map f)
  let fold_left f s = unop (P.fold_left f s) (T.fold_left f s)
  let fold_left2 f s = binop (P.fold_left2 f s) (T.fold_left2 f s)
  let move_if_affected ?(length=None) (ask:Q.ask) x v f = unop_to_t (fun x -> P.move_if_affected ~length:length ask x v f) (fun x -> T.move_if_affected ~length:length ask x v f) x
  let smart_join ?(length=None) f g = binop_to_t (P.smart_join ~length:length f g) (T.smart_join ~length:length f g)
  let smart_widen ?(length=None) f g = binop_to_t (P.smart_widen ~length:length f g) (T.smart_widen ~length:length f g)
  let smart_leq ?(length=None) f g = binop (P.smart_leq ~length:length f g) (T.smart_leq ~length:length f g)

  (* TODO: Check if these three are ok to make here *)
  let printXml f = unop (P.printXml f) (T.printXml f)
  let pretty_f _ = pretty
  let toXML_f _ = unop (P.toXML_f P.short) (T.toXML_f T.short)


  let pretty_diff () ((p1,t1),(p2,t2)) = match (p1, t1),(p2, t2) with
    | (Some p1, None), (Some p2, None) -> P.pretty_diff () (p1, p2)
    | (None, Some t1), (None, Some t2) -> T.pretty_diff () (t1, t2)
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  (* Functions that make us of the configuration flag *)
  let name () = "FlagConfiguredArrayDomain: " ^ if get_bool "exp.partition-arrays" then P.name () else T.name ()

  let bot () =
    if get_bool "exp.partition-arrays" then
      (Some (P.bot ()), None)
    else
      (None, Some (T.bot ()))

  let top () =
    if get_bool "exp.partition-arrays" then
      (Some (P.top ()), None)
    else
      (None, Some (T.top ()))

  let make i v =
    if get_bool "exp.partition-arrays" then
      (Some (P.make i v), None)
    else
      (None, Some (T.make i v))
end
