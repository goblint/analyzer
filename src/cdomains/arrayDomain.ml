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

  let map f x =
    f x 

  let fold_left f a x =
    f a x

  let fold_left2 f a x y =
    f a x y

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
end

module TrivialFragmented (Val: Lattice.S): S with type value = Val.t and type idx = ExpDomain.t =
struct
  let name () = "trivial fragmented arrays"
  module Expp = ExpDomain
  module Base = Lattice.Prod3 (Val) (Val) (Val)
  include Lattice.ProdSimple(Expp) (Base)
  type idx = ExpDomain.t
  type value = Val.t
  

  let short w (e,(xl, xm, xr)) = 
    if Expp.is_bot e then 
      "Array (no part.) : " ^ Val.short (w - 7) xl
    else
      "Array (part. by " ^ Expp.short (w-7) e ^ "): (" ^
        Val.short (w - 7) xl ^ " -- " ^
        Val.short (w - 7) xm ^ " -- " ^
        Val.short (w - 7) xr ^ ")"
        (* TODO: w-7 needs to be replaced here *)

  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m

  let array_should_join ?(length=None) (e1, _) (e2, _) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) =
    let one_bot = (Expp.is_bot e1) <> (Expp.is_bot e2) in  
    if one_bot then
      begin
        let non_bot_value = if Expp.is_bot e1 then e2 else e1 in
        let other_eval = if Expp.is_bot e1 then x_eval_int else y_eval_int in
        match non_bot_value with
        | `Top -> true
        | `Bot -> true (* does not happen *)
        | `Lifted exp -> 
          begin
            (* ask in the state of the other one for the value of the expression *)
            match other_eval exp with
            | Some x when Int64.equal x Int64.zero -> false
            | Some x -> begin 
                  match length with
                    | Some y when Int64.equal x (Int64.sub y Int64.one) -> false
                    | _ -> true
                  end 
            | _ -> true
          end 
      end
    else true

  let get (ask:Q.ask) (e, (xl, xm, xr)) i =
    Messages.report ("Array get@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ")");
    Printf.printf "Array get@%s (partitioned by %s)\n"  (Expp.short 20 i) (Expp.short 20 e);
    let join_over_all = Val.join (Val.join xl xm) xr in
    if Expp.is_bot e then (if Val.is_bot join_over_all then Val.top () else join_over_all)
    (* When the array is not partitioned, and all segments are \bot, we return \top.
    TODO: Check how that works with the case in which we want to get rid of the expression when we are at the end.
    Should not really cause any issues since in those cases the rest of the values would not be \bot *)
    (* TODO: Here we should warn if we read bot because this means we are reading unitialized values *)
    else
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
        | _ -> join_over_all (* The case in which we don't know anything *)


  let get_vars_in_e (e, _) =
  match e with
    | `Top
    | `Bot -> []
    | `Lifted exp -> Basetype.CilExp.get_vars exp

  let contains_globals e =  (* TODO: AddrOf things *)
    match e with
    | `Top
    | `Bot -> false
    | `Lifted exp ->
      let vars = Basetype.CilExp.get_vars exp in
      List.exists (fun x -> x.vglob) vars

  let map f (e, (xl, xm, xr)) =
    (e, (f xl, f xm, f xr))  

  let fold_left f a (_, ((xl:value), (xm:value), (xr:value))) =
    f (f (f a xl) xm) xr

  let fold_left2 f a (_, ((xl:value), (xm:value), (xr:value))) (_, ((yl:value), (ym:value), (yr:value))) =
    f (f (f a xl yl) xm ym) xr yr 
       
  let move_if_affected ?(length=None) (ask:Q.ask) ((e, (xl,xm, xr)) as x) (v:varinfo) movement_for_exp =
    let move (i:int option) =     (* TODO: Maybe it would be nicer to switch to some kind of enum here *)
      match i with
      | Some 0   -> 
        (e, (xl, xm, xr))
      | Some 1   -> 
        begin
          Messages.report ("moved - old was "^(short 20 (e, (xl, xm, xr)))^" , new is "^(short 20 (e, (Val.join xl xm, xr, xr)))^"\n") ; 
          (e, (Val.join xl xm, xr, xr)) (* moved one to the right *)
        end
      | Some -1  -> 
        (e, (xl, xl, Val.join xm xr)) (* moved one to the left  *)
      | Some x when x > 1 -> 
        (e, (Val.join (Val.join xl xm) xr, xr, xr)) (* moved more than one to the right *)
      | Some x when x < -1
        -> (e, (xl, xl, Val.join (Val.join xl xm) xr)) (* moved more than one to the left *)
      | _ ->
        begin
          Messages.report "Destructive assignment to expression, not covering entire array.";
          (* TODO: Is it necessary to take top here? *)
          top()
        end
    in
    match e with
    | `Lifted exp ->
        let is_affected = Basetype.CilExp.occurs v exp in
        if not is_affected then
          x
        else
          (* check if one covers the entire array, so we can drop partitioning *)
          begin
            Messages.warn "Checking if one partition covers entire array";
            let exp_value = 
              match e with
                | `Lifted e' -> 
                  begin
                    match ask (Q.EvalInt e') with
                    | `Int n -> Q.ID.to_int n
                    | _ -> None
                  end
              |_ -> None 
            in
            let e_equals_length =
              match length with
              | Some l -> BatOption.map_default (Int64.equal l) false exp_value
              | _ -> false
            in
            let e_equals_minus_one =
              BatOption.map_default (Int64.equal Int64.minus_one) false exp_value
            in
            if e_equals_length then
              begin
                Messages.report "Entire array is covered by left value, dropping partitioning.";
                Expp.bot(),(xl, xl, xl)
              end
            else if e_equals_minus_one then
              begin
                Messages.report "Entire array is covered by right value, dropping partitioning.";
                Expp.bot(),(xr, xr, xr)
              end
            else
              begin
                move (movement_for_exp exp) 
              end
          end
    | _ -> x (* If the array is not actually partitioned, there is nothing to do *)

  let set ?(length=None) (ask:Q.ask) (e, (xl, xm, xr)) i a =
    begin
      Messages.report ("Array set@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ")");
      let lubIfNotBot x = if Val.is_bot x then x else Val.join a x in
      if Expp.is_bot e then
        if contains_globals i then
          (* expressions containing globals are not suitable for partitioning *)
          let join_over_all = Val.join (Val.join xl xm) xr in
          let result = if Val.is_bot join_over_all then Val.top () else Val.join a join_over_all in
          (e, (result, result, result))
        else
          begin (* this should be solved via the must equal *)
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
            let join_over_all = Val.join (Val.join xl xm) xr in
            let top_if_bot_lub_otherwise = if Val.is_bot join_over_all then Val.top () else join_over_all in
            let l = if e_equals_zero then Val.bot () else top_if_bot_lub_otherwise in (* TODO: How does this play with partitioning again according to a different rule? *)
            let r = if e_equals_maxIndex then Val.bot () else top_if_bot_lub_otherwise in (* TODO: How does this play with partitioning again according to a different rule? *)
            Messages.report ("Array set@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ") - new value is" ^ short 50 (i, (l, a, r)) )
            ;(i, (l, a, r))
          end
      else
        begin
          Messages.warn ("e is " ^ (Expp.short 20 e) ^ ", i is " ^ (Expp.short 20 i));
          match e, i with
          | `Lifted e', `Lifted i' -> begin
              let isEqual = match ask (Q.MustBeEqual (e',i')) with
                | `Bool true -> true
                | _ -> false in
              if isEqual then
                begin
                  Messages.report ("Array set@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ") - new value is" ^ short 50 (e, (xl, a, xr)) );
                  (e, (xl, a, xr))
                end
              else
                begin
                  let left = match ask (Q.MayBeLess (i', e')) with        (* (may i < e) ? xl : bot *)
                  | `Bool false -> xl
                  | _ -> lubIfNotBot xl in
                  let middle = match ask (Q.MayBeEqual (i', e')) with      (* (may i = e) ? xm : bot *)
                  | `Bool false -> xm
                  | _ -> Val.join xm a in
                  let right =  match ask (Q.MayBeLess (e', i')) with    (* (may i > e) ? xr : bot *)
                  | `Bool false -> xr
                  | _ -> lubIfNotBot xr in
                  (e, (left, middle, right))
                end
            end
          | _ -> 
            (* If either the expression used to write or the partitioning is not known, all segements except the empty ones
               will be affected *)
            (e, (lubIfNotBot xl, Val.join xm a, lubIfNotBot xr))
        end
    end

  (* TODO: Do i really need to make this explicit? if the array is partitioned according to \top every read while have to take a least upper bound regardless of what the rest of the code does?! *)
  let join (e1, (xl1,xm1,xr1)) (e2, (xl2,xm2,xr2)) =
    let new_e = Expp.join e1 e2 in
    if Expp.is_top new_e then (* TODO: Figure out how this relates with what bottom means i.e. How does this play with partitioning again according to a different rule? *)
      let join_over_all = Val.join (Val.join (Val.join xl1 xm1) xr1) (Val.join (Val.join xl2 xm2) xr2) in
      (new_e, (join_over_all, join_over_all, join_over_all))
    else
      (new_e, (Val.join xl1 xl2, Val.join xm1 xm2, Val.join xr1 xr2))

  (* TODO: What if i =_must j? *)

  let make i v =
    if Val.is_bot v then (Expp.bot(), (Val.top(), Val.top(), Val.top()))
    else  (Expp.bot(), (v, v, v))
  (* TODO: We need to see whether we need to modify the bottom element from the Prod3 domain here *)
  (* TODO: What about the cases where this is called with v != \bot, are we still sound in those *)
  (* TODO: Interaction with get and the catch all *)

  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f (e, (xl, xm, xr)) = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml xl
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

  let map f (x, l):t =
    (Base.map f x, l)

  let fold_left f a (x, l) =
    Base.fold_left f a x

  let fold_left2 f a (x, l) (y, l) =
    Base.fold_left2 f a x y

  let get_vars_in_e _ = []
end


module TrivialFragmentedWithLength (Val: Lattice.S): S with type value = Val.t and type idx = ExpDomain.t =
struct
  module Base = TrivialFragmented (Val)
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

  let map f (x, l):t =
    (Base.map f x, l)

  let fold_left f a (x, l) =
    Base.fold_left f a x  

  let fold_left2 f a (x, l) (y, l) =
    Base.fold_left2 f a x y


  let get_vars_in_e (x, _) = Base.get_vars_in_e x
end
