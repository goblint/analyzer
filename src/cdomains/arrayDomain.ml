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
  val set: ?length:(int64 option)  -> Q.ask -> t -> idx -> value -> t
  val make: int -> value -> t
  val length: t -> int option

  val get_e: t -> idx option
  val get_vars_in_e: t -> Cil.varinfo list
  val is_affected_by: t -> Cil.varinfo -> bool
  val move: t -> int -> t
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

  let get_e _ = None
  let is_affected_by _ _ = false
  let move a _ = a
  let get_vars_in_e _ = []

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
  

  let short w (e,(xl, xm, xr)) = "Array (partitioned by " ^ Expp.short (w-7) e ^ "): (" ^
                                 Val.short (w - 7) xl ^ " -- " ^ Val.short (w - 7) xm ^ " -- "
                                 ^ Val.short (w - 7) xr ^ ")"
                                (* TODO w-7 needs to be replaced here *)

  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m

  (* TODO For set&get we later need to distinguish between must & may equality to see *)
  (* decide whether to apply a least upper bound or not *)

  let get (ask:Q.ask) (e, (xl, xm, xr)) i =
    Messages.report ("Array get@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ")");
    let join_over_all = Val.join (Val.join xl xm) xr in
    if Expp.is_bot e then (if Val.is_bot join_over_all then Val.top () else join_over_all)
    (* When the array is not partitioned, and all segments are \bot, we return \top.
    TODO: Check how that works with the case in which we want to get rid of the expression when we are at the end.
    Should not really cause any issues since in those cases the rest of the values would not be \bot *)
    else
      match e, i with
        | `Lifted e', `Lifted i' ->
          begin
            let isEqual = match ask (Q.MustBeEqual (e',i')) with
              | `Bool x when x == true -> true
              | _ -> false in
            if isEqual then xm
            (* TODO: else if all the other ways in which e and i might relate *)
            else join_over_all
          end
        | _ -> join_over_all (* The case in which we don't know anything *)


  let get_e (e, _) = Some e (* TODO:This looks like it should really not be here,
                               we should probably do all that internally *)


  (* TODO: both possibilities to use as underlying element here have support for pulling this in  *)
  let get_vars_in_e (e, _) = (* TODO: Maybe move this inward even further by putting it in ExprDomain *)
    let rec varsInExp exp = match exp with
      | Const _
      | SizeOf _
      | SizeOfE _
      | AlignOfE _
      | AddrOfLabel _
      | SizeOfStr _
      | AlignOf _
      | Question _ (* TODO is this correct? *)
      | AddrOf _
      | StartOf _ -> []
      | UnOp (_, e, _ )
      | CastE (_, e) -> varsInExp e
      | BinOp (_, e1, e2, _) -> (varsInExp e1)@(varsInExp e2)
      | Lval (Var v, _) -> [v]
      | Lval (Mem _,_) -> [] in
    match e with
    | `Top
    | `Bot -> []
    | `Lifted exp -> varsInExp exp


  let is_affected_by (e, (xl,xm,xr)) v =
    let vars = get_vars_in_e (e, (xl,xm,xr)) in
    List.exists (fun x -> x ==v) vars


  let set ?(length=None) (ask:Q.ask) (e, (xl, xm, xr)) i a =
    begin
      Messages.report ("Array set@" ^ (Expp.short 20 i) ^ " (partitioned by " ^ (Expp.short 20 e) ^ ")");
      let lub = Val.join a in
      if Expp.is_bot e then
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
          let e_equals_zero = 
              match exp_value with
                | Some v -> Int64.equal v Int64.zero
                | _ -> false
          in
          let e_equals_maxIndex = 
            match length with
            | Some l ->
              begin
                match exp_value with
                | Some v -> Int64.equal v (Int64.sub l Int64.one)
                | _ -> false
              end
            | _ -> false
          in
          let join_over_all = Val.join (Val.join xl xm) xr in
          let top_if_bot_lub_otherwise = if Val.is_bot join_over_all then Val.top () else join_over_all in
          let l = if e_equals_zero then Val.bot () else top_if_bot_lub_otherwise in (* TODO: How does this play with partitioning again according to a different rule? *)
          let r = if e_equals_maxIndex then Val.bot () else top_if_bot_lub_otherwise in (* TODO: How does this play with partitioning again according to a different rule? *)
          (i, (l, a, r))
        end
      else
        begin
          Messages.warn ("e is " ^ (Expp.short 20 e) ^ ", i is " ^ (Expp.short 20 i));
          if Expp.equal e i then (e, (xl, a, xr))
          (* TODO: else if all the other cases *)
          else (e, (lub xl, lub xm, lub xr));
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

  let make i v = (Expp.bot(), (Val.bot(), v, Val.bot()))
  (* TODO: We need to see whether we need to modify the bottom element from the Prod3 domain here *)
  (* TODO: It would also seem we need to provide the expression that we are suing to split it here *)
  (* TODO: WTF is going on here? This better be only called with v = \bot *)
  (* TODO: Interaction with get and the catch all *)

  let length _ = None

  let move (e, (xl, xm, xr)) (i:int) =     (* Under the assumption that we always get exact information about how much it moved *)
    match i with
    | 0   -> (e, (xl, xm, xr))
    | 1   -> Messages.report ("moved - old was "^(short 20 (e, (xl, xm, xr)))^" , new is "^(short 20 (e, (Val.join xl xm, xr, xr)))^"\n") ; (e, (Val.join xl xm, xr, xr)) (* moved one to the right *)
    | -1  -> (e, (xl, xl, Val.join xm xr)) (* moved one to the left  *)
    | _ when i > 1
      -> (e, (Val.join (Val.join xl xm) xr, xr, xr)) (* moved more than one to the right *)
    | _ when i < -1
      -> (e, (xl, xl, Val.join (Val.join xl xm) xr)) (* moved more than one to the left *)
    | _ -> top()

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

  let is_affected_by _ _ = false
  let get_e _ = None
  let move x _ = x
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
    let new_l = IntDomain.Flattened.to_int l in
    Base.set ~length:new_l ask x i v, l 
  let make l x = Base.make l x, Length.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Length.to_int l)

  let is_affected_by (x, _) v = Base.is_affected_by x v
  let get_e (x, _) = Base.get_e x
  let move (x, l) i = (Base.move x i, l)
  let get_vars_in_e (x, _) = Base.get_vars_in_e x
end
