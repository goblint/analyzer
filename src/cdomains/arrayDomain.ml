open Pretty
open Cil
open GobConfig

module M = Messages
module A = Array
module Q = Queries

module type S =
sig
  include Lattice.S
  type idx
  type value

  val get: Q.ask -> t -> ExpDomain.t * idx -> value
  val set: Q.ask -> t -> ExpDomain.t * idx -> value -> t
  val make: idx -> value -> t
  val length: t -> idx option

  val move_if_affected: ?replace_with_const:bool -> Q.ask -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
  val get_vars_in_e: t -> Cil.varinfo list
  val map: (value -> value) -> t -> t
  val fold_left: ('a -> value -> 'a) -> 'a -> t -> 'a
  val fold_left2: ('a -> value -> value -> 'a) -> 'a -> t -> t -> 'a
  val smart_join: (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> t
  val smart_widen: (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> t
  val smart_leq: (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> bool
  val update_length: idx -> t -> t
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
  let set (ask: Q.ask) a i v = join a v
  let make i v = v
  let length _ = None

  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let get_vars_in_e _ = []
  let map f x = f x
  let fold_left f a x = f a x
  let fold_left2 f a x y = f a x y

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq
  let update_length _ x = x
end

(** Special signature so that we can use the _with_length functions from PartitionedWithLength but still match the interface *
  * defined for array domains *)
module type SPartitioned =
sig
  include S
  val set_with_length: idx option -> Q.ask -> t -> ExpDomain.t * idx -> value -> t
  val smart_join_with_length: idx option -> (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> t
  val smart_widen_with_length: idx option -> (exp -> int64 option) -> (exp -> int64 option)  -> t -> t-> t
  val smart_leq_with_length: idx option -> (exp -> int64 option) -> (exp -> int64 option) -> t -> t -> bool
  val move_if_affected_with_length: ?replace_with_const:bool -> idx option -> Q.ask -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
end

module Partitioned (Val: LatticeWithSmartOps) (Idx:IntDomain.S):SPartitioned with type value = Val.t and type idx = Idx.t =
struct
  (* Contrary to the description in Michael's master thesis, abstract values here always have the form *)
  (* (Expp, (Val, Val, Val)). Expp is top when the array is not partitioned. In these cases all three  *)
  (* values from Val are identical *)
  module Expp = ExpDomain
  module Base = Lattice.Prod3 (Val) (Val) (Val)
  include Lattice.ProdSimple(Expp) (Base)

  type idx = Idx.t
  type value = Val.t

  let name () = "partitioned array"

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

  let printXml f ((e, (xl, xm, xr)) as x) =
    if is_not_partitioned x then
      let join_over_all = Val.join (Val.join xl xm) xr in
      BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml join_over_all
    else
      BatPrintf.fprintf f "<value>\n<map>\n
          <key>Partitioned By</key>\n%a\n
          <key>l</key>\n%a\n\n
          <key>m</key>\n%a\n\n
          <key>r</key>\n%a\n\n
        </map></value>\n" Expp.printXml e Val.printXml xl Val.printXml xm Val.printXml xr

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

  let move_if_affected_with_length ?(replace_with_const=false) length (ask:Q.ask) ((e, (xl,xm, xr)) as x) (v:varinfo) movement_for_exp =
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
          let default = (Expp.top (), (nval, nval, nval)) in
          if replace_with_const then
            match e with
            | `Lifted e' ->
              begin
                match ask (Q.EvalInt e') with
                | `Int n ->
                  begin
                    match Q.ID.to_int n with
                    | Some i ->
                      (`Lifted (Cil.kinteger64 IInt i), (xl, xm, xr))
                    | _ -> default
                  end
                | _ -> default
              end
            | _ -> default
          else
            default
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
                  match Idx.to_int l with
                  | Some i ->
                    begin
                      match ask (Q.MayBeLess (exp, Cil.kinteger64 Cil.IInt i)) with
                      | `Bool false -> true (* !(e <_{may} length) => e >=_{must} length *)
                      | _ -> false
                    end
                  | None -> false
                end
              | _ -> false
            in
            let e_must_less_zero =
              match ask (Q.MayBeLess (Cil.mone, exp)) with
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

  let move_if_affected ?(replace_with_const=false) = move_if_affected_with_length ~replace_with_const:replace_with_const None

  let set_with_length length (ask:Q.ask) ((e, (xl, xm, xr)) as x) (i,_) a =
    let use_last = get_string "exp.partition-arrays.keep-expr" = "last" in
    let exp_value e =
      match e with
      | `Lifted e' ->
          begin
            match ask (Q.EvalInt e') with
            | `Int n -> Q.ID.to_int n
            | _ -> None
          end
      |_ -> None
    in
    let equals_zero e = BatOption.map_default (Int64.equal Int64.zero) false (exp_value e) in
    let equals_maxIndex e =
      match length with
      | Some l ->
        begin
          match Idx.to_int l with
          | Some i -> BatOption.map_default (Int64.equal (Int64.sub i Int64.one)) false (exp_value e)
          | None -> false
        end
      | _ -> false
    in
    let lubIfNotBot x = if Val.is_bot x then x else Val.join a x in
    if is_not_partitioned x then
      if not_allowed_for_part i then
        let result = Val.join a (join_of_all_parts x) in
        (e, (result, result, result))
      else
        let l = if equals_zero i then Val.bot () else join_of_all_parts x in
        let r = if equals_maxIndex i then Val.bot () else join_of_all_parts x in
        (i, (l, a, r))
    else
      let isEqual e' i' = match ask (Q.MustBeEqual (e',i')) with
        | `Bool true -> true
        | _ -> false
      in
      match e, i with
      | `Lifted e', `Lifted i' when not use_last || not_allowed_for_part i -> begin
          let default =
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
          in
          if isEqual e' i' then
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
      | `Lifted e', `Lifted i' ->
        if isEqual e' i' then
          (e,(xl,a,xr))
        else
          let left = if equals_zero i then Val.bot () else Val.join xl @@ Val.join
            (match ask (Q.MayBeEqual (e', i')) with
            | `Bool false -> Val.bot()
            | _ -> xm) (* if e' may be equal to i', but e' may not be smaller than i' then we only need xm *)
            (
              match ask (Q.MustBeEqual(BinOp(PlusA, e', Cil.integer 1, Cil.intType),i')) with
              | `Bool true -> xm
              | _ ->
                begin
                  match ask (Q.MayBeLess (e', i')) with
                  | `Bool false -> Val.bot()
                  | _ -> Val.join xm xr (* if e' may be less than i' then we also need xm for sure *)
                end
            )
          in
          let right = if equals_maxIndex i then Val.bot () else  Val.join xr @@  Val.join
            (match ask (Q.MayBeEqual (e', i')) with
            | `Bool false -> Val.bot()
            | _ -> xm)

            (
              match ask (Q.MustBeEqual(BinOp(PlusA, e', Cil.integer (-1), Cil.intType),i')) with
              | `Bool true -> xm
              | _ ->
                begin
                  match ask (Q.MayBeLess (i', e')) with
                  | `Bool false -> Val.bot()
                  | _ -> Val.join xl xm (* if e' may be less than i' then we also need xm for sure *)
                end
            )
          in
          (* The new thing is partitioned according to i so we can strongly update *)
          (i,(left, a, right))
      | _ ->
        (* If the expression used to write is not known, all segments except the empty ones will be affected *)
        (e, (lubIfNotBot xl, Val.join xm a, lubIfNotBot xr))

  let set = set_with_length None

  let join ((e1, (xl1,xm1,xr1)) as x1) ((e2, (xl2,xm2,xr2)) as x2) =
    let new_e = Expp.join e1 e2 in
    if Expp.is_top new_e then
      (* At least one of them was not partitioned, or e <> f *)
      let join_over_all = Val.join (join_of_all_parts x1) (join_of_all_parts x2) in
      (new_e, (join_over_all, join_over_all, join_over_all))
    else
      (new_e, (Val.join xl1 xl2, Val.join xm1 xm2, Val.join xr1 xr2))

  (* leq needs not be given explicitly, leq from product domain works here *)

  let make i v =
    if Idx.to_int i = Some Int64.one then
      (`Lifted (Cil.integer 0), (Val.bot (), v, Val.bot ()))
    else if Val.is_bot v then
      (Expp.top(), (Val.top(), Val.top(), Val.top()))
    else
      (Expp.top(), (v, v, v))

  let length _ = None

  let set_inplace = set
  let copy a = a

  let smart_op (op: Val.t -> Val.t -> Val.t) length ((e1, (xl1,xm1,xr1)) as x1) ((e2, (xl2,xm2,xr2)) as x2) x1_eval_int x2_eval_int =
    let must_be_length_minus_one v = match length with
      | Some l ->
        begin
          match Idx.to_int l with
          | Some i ->
            v = Some (Int64.sub i Int64.one)
          | None -> false
        end
      | None -> false
    in
    let must_be_zero v = v = Some Int64.zero in
    let op_over_all = op (join_of_all_parts x1) (join_of_all_parts x2) in
    match e1, e2 with
    | `Lifted e1e, `Lifted e2e when Basetype.CilExp.equal e1e e2e ->
      (e1, (op xl1 xl2, op xm1 xm2, op xr1 xr2))
    | `Lifted e1e, `Lifted e2e ->
      if get_string "exp.partition-arrays.keep-expr" = "last" || get_bool "exp.partition-arrays.smart-join" then
        let over_all_x1 = op (op xl1 xm1) xr1 in
        let over_all_x2 = op (op xl2 xm2) xr2 in
        let e1e_in_state_of_x2 = x2_eval_int e1e in
        let e2e_in_state_of_x1 = x1_eval_int e2e in
        let e1e_is_better = (not (Cil.isConstant e1e) && Cil.isConstant e2e) || Basetype.CilExp.compareExp e1e e2e < 0 in
        if e1e_is_better then (* first try if the result can be partitioned by e1e *)
          if must_be_zero e1e_in_state_of_x2  then
            (e1, (xl1, op xm1 over_all_x2, op xr1 over_all_x2))
          else if must_be_length_minus_one e1e_in_state_of_x2  then
            (e1, (op xl1 over_all_x2, op xm1 over_all_x2, xr1))
          else if must_be_zero e2e_in_state_of_x1 then
            (e2, (xl2, op over_all_x1 xm2, op over_all_x1 xr2))
          else if must_be_length_minus_one e2e_in_state_of_x1 then
            (e2, (op over_all_x1 xl2, op over_all_x1 xm2, xr2))
          else
            (Expp.top (), (op_over_all, op_over_all, op_over_all))
        else  (* first try if the result can be partitioned by e2e *)
          if must_be_zero e2e_in_state_of_x1 then
            (e2, (xl2, op over_all_x1 xm2, op over_all_x1 xr2))
          else if must_be_length_minus_one e2e_in_state_of_x1 then
            (e2, (op over_all_x1 xl2, op over_all_x1 xm2, xr2))
          else if must_be_zero e1e_in_state_of_x2 then
            (e1, (xl1, op xm1 over_all_x2, op xr1 over_all_x2))
          else if must_be_length_minus_one e1e_in_state_of_x2 then
            (e1, (op xl1 over_all_x2, op xm1 over_all_x2, xr1))
          else
            (Expp.top (), (op_over_all, op_over_all, op_over_all))
      else
        (Expp.top (), (op_over_all, op_over_all, op_over_all))
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

  let smart_join_with_length length x1_eval_int x2_eval_int x1 x2 =
    smart_op (Val.smart_join x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_widen_with_length length x1_eval_int x2_eval_int x1 x2  =
    smart_op (Val.smart_widen x1_eval_int x2_eval_int) length x1 x2 x1_eval_int x2_eval_int

  let smart_leq_with_length length x1_eval_int x2_eval_int ((e1, (xl1,xm1,xr1)) as x1) (e2, (xl2, xm2, xr2)) =
    let leq' = Val.smart_leq x1_eval_int x2_eval_int in
    let must_be_zero v = (v = Some Int64.zero) in
    let must_be_length_minus_one v =  match length with
      | Some l ->
        begin
          match Idx.to_int l with
          | Some i ->
            v = Some (Int64.sub i Int64.one)
          | None -> false
        end
      | None -> false
    in
    match e1, e2 with
    | `Top, `Top ->
      (* Those asserts ensure that for both arguments all segments are equal (as it should be) *)
      assert(Val.leq xl1 xm1); assert(Val.leq xm1 xr1); assert(Val.leq xl2 xm2); assert(Val.leq xm2 xr2);
      assert(Val.leq xm1 xl1); assert(Val.leq xr1 xm1); assert(Val.leq xm2 xl2); assert(Val.leq xr2 xm2);
      leq' (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2))    (* TODO: should the inner joins also be smart joins? *)
    | `Lifted _, `Top -> leq' (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2))
    | `Lifted e1e, `Lifted e2e ->
      if Basetype.CilExp.equal e1e e2e then
        leq' xl1 xl2 && leq' xm1 xm2 && leq' xr1 xr2
      else if must_be_zero (x1_eval_int e2e) then
        (* A read will never be from xl2 -> we can ignore that here *)
        let l = join_of_all_parts x1 in
        leq' l xm2 && leq' l xr2
      else if must_be_length_minus_one (x1_eval_int e2e) then
        (* A read will never be from xr2 -> we can ignore that here *)
        let l = join_of_all_parts x1 in
        leq' l xl2 && leq' l xm2
      else
        false
    | `Top, `Lifted e2e ->
      if must_be_zero (x1_eval_int e2e) then
        leq' xm1 xm2 && leq' xr1 xr2
      else if must_be_length_minus_one (x1_eval_int e2e) then
        leq' xl1 xl2 && leq' xm1 xm2
      else
        false
    | _ ->
      failwith "ArrayDomain: Unallowed state (one of the partitioning expressions is bot)"

  let smart_join = smart_join_with_length None
  let smart_widen = smart_widen_with_length None
  let smart_leq = smart_leq_with_length None

  let update_length _ x = x
end


module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Trivial (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t
  let get (ask: Q.ask) (x ,l) i = Base.get ask x i (* TODO check if in-bounds *)
  let set (ask: Q.ask) (x,l) i v = Base.set ask x i v, l
  let make l x = Base.make l x, l
  let length (_,l) = Some l

  let move_if_affected ?(replace_with_const=false) _ x _ _ = x
  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
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

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y
end


module PartitionedWithLength (Val: LatticeWithSmartOps) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Partitioned (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t
  let get ask (x,l) i = Base.get ask x i (* TODO check if in-bounds *)
  let set ask (x,l) i v = Base.set_with_length (Some l) ask x i v, l
  let make l x = Base.make l x, l
  let length (_,l) = Some l

  let move_if_affected ?(replace_with_const=false) ask (x,l) v i =
    (Base.move_if_affected_with_length ~replace_with_const:replace_with_const (Some l) ask x v i), l

  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
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

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (Base.name ())) Base.printXml x "length" Idx.printXml y
end

module FlagConfiguredArrayDomain(Val: LatticeWithSmartOps) (Idx:IntDomain.S):S with type value = Val.t and type idx = Idx.t =
struct
  module P = PartitionedWithLength(Val)(Idx)
  module T = TrivialWithLength(Val)(Idx)

  type idx = Idx.t
  type value = Val.t
  type t = P.t option * T.t option [@@deriving to_yojson]

  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"

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
  let get a x (e,i) = unop (fun x ->
        if e = `Top then
          let e' = BatOption.map_default (fun x -> `Lifted (Cil.kinteger64 IInt x)) (`Top) (Idx.to_int i) in
          P.get a x (e', i)
        else
          P.get a x (e, i)
      ) (fun x -> T.get a x (e,i)) x
  let set (ask:Q.ask) x i a = unop_to_t (fun x -> P.set ask x i a) (fun x -> T.set ask x i a) x
  let length = unop P.length T.length
  let get_vars_in_e = unop P.get_vars_in_e T.get_vars_in_e
  let map f = unop_to_t (P.map f) (T.map f)
  let fold_left f s = unop (P.fold_left f s) (T.fold_left f s)
  let fold_left2 f s = binop (P.fold_left2 f s) (T.fold_left2 f s)
  let move_if_affected ?(replace_with_const=false) (ask:Q.ask) x v f = unop_to_t (fun x -> P.move_if_affected ~replace_with_const:replace_with_const ask x v f) (fun x -> T.move_if_affected ~replace_with_const:replace_with_const ask x v f) x
  let smart_join f g = binop_to_t (P.smart_join f g) (T.smart_join f g)
  let smart_widen f g = binop_to_t (P.smart_widen f g) (T.smart_widen f g)
  let smart_leq f g = binop (P.smart_leq f g) (T.smart_leq f g)

  (* TODO: Check if these three are ok to make here *)
  let printXml f = unop (P.printXml f) (T.printXml f)
  let pretty_f _ = pretty
  let toXML_f _ = unop (P.toXML_f P.short) (T.toXML_f T.short)

  let update_length newl x = unop_to_t (P.update_length newl) (T.update_length newl) x

  let pretty_diff () ((p1,t1),(p2,t2)) = match (p1, t1),(p2, t2) with
    | (Some p1, None), (Some p2, None) -> P.pretty_diff () (p1, p2)
    | (None, Some t1), (None, Some t2) -> T.pretty_diff () (t1, t2)
    | _ -> failwith "FlagConfiguredArrayDomain received a value where not exactly one component is set"

  (* Functions that make us of the configuration flag *)
  let name () = "FlagConfiguredArrayDomain: " ^ if get_bool "exp.partition-arrays.enabled" then P.name () else T.name ()

  let partition_enabled () =
    if get_bool "exp.partition-arrays.enabled" then
      if get_bool "exp.fast_global_inits" then
        failwith "Options 'exp.partition-arrays.enabled' and 'exp.fast_global_inits' are incompatible. Disable at least one of them."
      else
        true
    else
      false

  let bot () =
    if partition_enabled () then
      (Some (P.bot ()), None)
    else
      (None, Some (T.bot ()))

  let top () =
    if partition_enabled () then
      (Some (P.top ()), None)
    else
      (None, Some (T.top ()))

  let make i v =
    if partition_enabled () then
      (Some (P.make i v), None)
    else
      (None, Some (T.make i v))
end
