open Pretty
open Cil

module M = Messages
module A = Array
module GU = Goblintutil
module Q = Queries

module type S =
sig
  include Lattice.S
  type idx
  type value

  val get: Q.ask -> t -> idx -> value
  val set: ?length:(int64 option) -> Q.ask -> t -> idx -> value -> t
  val make: int -> value -> t
  val length: t -> int option

  val array_should_join: ?length:(int64 option) -> t -> t -> (exp -> int64 option) -> (exp -> int64 option) -> bool 
  val move_if_affected: ?length:(int64 option) -> Q.ask -> t -> Cil.varinfo -> (Cil.exp -> int option) -> t
  val get_vars_in_e: t -> Cil.varinfo list
  val map: (value -> value) -> t -> t
  val fold_left: ('a -> value -> 'a) -> 'a -> t -> 'a
  val fold_left2: ('a -> value -> value -> 'a) -> 'a -> t -> t -> 'a
  val smart_join: ?length:(int64 option) -> t -> t -> (exp -> int64 option) -> (exp -> int64 option) -> t
  val smart_widen: ?length:(int64 option) -> t -> t -> (exp -> int64 option) -> (exp -> int64 option) -> t
  val smart_leq: ?length:(int64 option) -> t -> t -> (exp -> int64 option) -> (exp -> int64 option) -> bool
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

  let array_should_join ?(length = None) _ _ _ _ = true
  let move_if_affected ?(length = None) _ x _ _ = x
  let get_vars_in_e _ = []
  let map f x = f x 
  let fold_left f a x = f a x
  let fold_left2 f a x y = f a x y

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
  let smart_join ?(length=None) a b _ _ = join a b
  let smart_widen ?(length=None) a b _ _ = widen a b
  let smart_leq ?(length=None) a b _ _ = leq a b
end

module Partitioned (Val: Lattice.S): S with type value = Val.t and type idx = ExpDomain.t =
struct
  (* Contrary to the description in Michael's master thesis, abstract values here always have the form *)
  (* (Expp, (Val, Val, Val)). Expp is top when the array is not partitioned. In these cases all three  *)
  (* values from Val are identical *)

  let name () = "partitioned arrays"
  module Expp = ExpDomain
  module Base = Lattice.Prod3 (Val) (Val) (Val)
  include Lattice.ProdSimple(Expp) (Base)
  type idx = ExpDomain.t
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

  let array_should_join ?(length=None) ((e1, _) as x1) ((e2, _) as x2) x_eval_int y_eval_int =
    let exactly_one_not_part = (is_not_partitioned x1) <> (is_not_partitioned x2) in  
    if exactly_one_not_part then
      let partitioning_exp = if is_not_partitioned x1 then e2 else e1 in
      let other_eval = if is_not_partitioned x1 then x_eval_int else y_eval_int in
      match partitioning_exp with
      | `Top -> true
      | `Bot -> true (* does not happen *)
      | `Lifted exp -> 
        begin
          (* ask in the state of the other one for the value of the expression *)
          match other_eval exp with
          | Some x when Int64.equal x Int64.zero -> false
          | Some x ->
            begin 
              match length with
              | Some y when Int64.equal x (Int64.sub y Int64.one) -> false
              | _ -> true
            end 
          | _ -> true
        end 
    else true

  let get (ask:Q.ask) ((e, (xl, xm, xr)) as x) i =
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
    | `Bot -> false
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

  let set ?(length=None) (ask:Q.ask) ((e, (xl, xm, xr)) as x) i a =
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
    if i = 1 then (`Lifted (Cil.integer 0), (Val.bot (), v, Val.bot ())) 
    else
    if Val.is_bot v then (Expp.top(), (Val.top(), Val.top(), Val.top()))
    else  (Expp.top(), (v, v, v))

  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f (e, (xl, xm, xr)) =
    let join_over_all = Val.join (Val.join xl xm) xr in
    BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml join_over_all

  let smart_op (op: Val.t -> Val.t -> Val.t) length ((e1, (xl1,xm1,xr1)) as x1) ((e2, (xl2,xm2,xr2)) as x2) x1_eval_int x2_eval_int =
    let op_over_all = op (join_of_all_parts x1) (join_of_all_parts x2) in
    match e1, e2 with
    | `Lifted e1e, `Lifted e2e when Basetype.CilExp.equal e1e e2e ->
      (* Partitioned according to the same expression, join segment-wise *)
      (e1, (op xl1 xl2, op xm1 xm2, op xr1 xr2))
    | `Lifted _, `Lifted _
    | `Top, `Top ->
      (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | `Top, `Lifted e2e ->
      let must_be_zero = (x1_eval_int e2e = Some Int64.zero) in
      if must_be_zero then
        let () = Printf.printf "smart op!!!!!  -> %s\n " (short 80 (e2, (xl2, op xm1 xm2, op xr1 xr2))) in 
        (e2, (xl2, op xm1 xm2, op xr1 xr2))
      else
        let must_be_size_minus_one = match length with
          | Some l -> x1_eval_int e2e = Some (Int64.sub l Int64.one)
          | None -> false
        in
        if must_be_size_minus_one then
          (e2, (Val.join xl1 xl2, Val.join xm1 xm2, xr2))
        else
          (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | `Lifted e1e, `Top ->
      let must_be_zero = (x2_eval_int e1e = Some Int64.zero) in
      if must_be_zero then
        let () = Printf.printf "smart op!!!!!   ->  %s\n" (short 80 (e1, (xl1, op xm1 xm2, op xr1 xr2))) in 
        (e1, (xl1, op xm1 xm2, op xr1 xr2))
      else
        let must_be_size_minus_one = match length with
          | Some l -> x2_eval_int e1e = Some (Int64.sub l Int64.one)
          | None -> false
        in
        if must_be_size_minus_one then
          (e1, (Val.join xl1 xl2, Val.join xm1 xm2, xr1))
        else
          (Expp.top (), (op_over_all, op_over_all, op_over_all))
    | _ ->
      assert(false);
      (Expp.top (), (op_over_all, op_over_all, op_over_all))
  
  
  let smart_join ?(length=None) x1 x2 x1_eval_int x2_eval_int =
    smart_op Val.join length x1 x2 x1_eval_int x2_eval_int

  let smart_widen ?(length=None) x1 x2 x1_eval_int x2_eval_int =
    smart_op Val.widen length x1 x2 x1_eval_int x2_eval_int

  let smart_leq ?(length=None) (e1, (xl1,xm1,xr1)) (e2, (xl2, xm2, xr2)) x1_eval_int x2_eval_int =
    match e1, e2 with
    | `Top, `Top
    | `Lifted _, `Top -> Val.leq (Val.join xl1 (Val.join xm1 xr1)) (Val.join xl2 (Val.join xm2 xr2))
    | `Lifted e1e, `Lifted e2e -> Basetype.CilExp.equal e1e e2e && Val.leq xl1 xl2 && Val.leq xm1 xm2 && Val.leq xr1 xr2
    | `Top, `Lifted e2e ->
      let must_be_zero = (x1_eval_int e2e = Some Int64.zero) in
      if must_be_zero then
        Val.leq xm1 xm2 && Val.leq xr1 xr2
      else
        let must_be_size_minus_one = match length with
          | Some l -> x1_eval_int e2e = Some (Int64.sub l Int64.one)
          | None -> false
        in
        if must_be_size_minus_one then
          Val.leq xl1 xl2 && Val.leq xm1 xm2
        else
          false
    | _ -> assert(false); false
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

  let array_should_join ?(length =None) _ _ _ _ = true
  let move_if_affected ?(length = None) _ x _ _ = x
  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
  let get_vars_in_e _ = []

  let smart_join ?(length=None) a b _ _ = join a b
  let smart_widen ?(length=None) a b _ _ = widen a b
  let smart_leq ?(length=None) a b _ _ = leq a b
end


module PartitionedWithLength (Val: Lattice.S): S with type value = Val.t and type idx = ExpDomain.t =
struct
  module Base = Partitioned (Val)
  module Length = IntDomain.Flattened (* We only keep one exact value or top/bot here *)
  include Lattice.Prod (Base) (Length)
  type idx = ExpDomain.t
  type value = Val.t
  let get ask (x,l) i = Base.get ask x i (* TODO check if in-bounds *)
  let set ?(length=None) ask (x,l) i v =
    let new_l = Length.to_int l in
    Base.set ~length:new_l ask x i v, l 
  let make l x = Base.make l x, Length.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Length.to_int l)

  let array_should_join ?(length=None) (x, xl) (y, yl) x_eval y_eval = 
    let new_l = Length.to_int xl in
    Base.array_should_join ~length:new_l x y x_eval y_eval

  let move_if_affected ?(length = None) ask (x,l) v i = 
    let new_l = Length.to_int l in
    (Base.move_if_affected ~length:new_l ask x v i), l

  let map f (x, l):t = (Base.map f x, l)
  let fold_left f a (x, l) = Base.fold_left f a x  
  let fold_left2 f a (x, l) (y, l) = Base.fold_left2 f a x y
  let get_vars_in_e (x, _) = Base.get_vars_in_e x

  let smart_join ?(length=None) (x,xl) (y,yl) x_eval_int y_eval_int = 
    let new_l = Length.to_int xl in
    (Base.smart_join ~length:new_l x y x_eval_int y_eval_int,Length.join xl yl)

  let smart_widen ?(length=None) (x,xl) (y,yl) x_eval_int y_eval_int = 
    let new_l = Length.to_int xl in
    (Base.smart_widen ~length:new_l x y x_eval_int y_eval_int,Length.join xl yl)

  let smart_leq ?(length=None) (x,xl) (y,yl) x_eval_int y_eval_int =
    let new_l = Length.to_int xl in
    Base.smart_leq ~length:new_l x y x_eval_int y_eval_int
end
