(** domain of the base analysis *)

open Cil

module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let arrays_should_join (x:t) (y:t) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) = true
    (* let array_join_ok key (value:VD.t) =
      try 
        let other = find key y in
        VD.array_should_join value other x_eval_int y_eval_int
      with Not_found -> true
    in
    for_all array_join_ok x *)

  let smart_join (x:t) (y:t) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) =
    let normal_join = join x y in
    if is_top normal_join || is_bot normal_join then
      normal_join
    else
      if x == y then x else long_map2 (fun (a:VD.t) (b:VD.t) -> VD.smart_join a b (x_eval_int) (y_eval_int)) x y

  let smart_widen (x:t) (y:t) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) =
    let normal_widen = widen x y in
    if is_top normal_widen || is_bot normal_widen then
      normal_widen
    else
      if x == y then x else long_map2 (fun (a:VD.t) (b:VD.t) -> VD.smart_widen a b (x_eval_int) (y_eval_int)) x y


  let smart_leq (x:t) (y:t) (x_eval_int: exp -> int64 option) (y_eval_int: exp -> int64 option) =
    if is_top y then true
    else if is_bot x then true
    else if is_bot y then is_bot x
    else if is_top x then is_top y (* at this point, we have dealt with all _|_ and T *)
    else
    (* For each key-value in m1, the same key must be in m2 with a geq value: *)
      let p key value =
        try VD.smart_leq value (find key y) (x_eval_int) (y_eval_int) with Not_found -> false
      in
      x == y || for_all p x



  let name () = "value domain"
end

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end

let heap_hash = Hashtbl.create 113

let get_heap_var loc =
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
    let newvar = makeGlobalVar name voidType in
    Hashtbl.add heap_hash loc newvar;
    newvar

module Glob =
struct
  module Var = Basetype.Variables
  module Val = VD
end


module VarSet = SetDomain.Make(Basetype.Variables)
module VarMap = MapDomain.MapBot_LiftTop(Basetype.Variables)(VarSet)

module type ExpEvaluator =
sig
  val eval_exp: CPA.t * Flag.t * VarMap.t ->  Cil.exp -> int64 option
end

(* Takes a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor(ExpEval:ExpEvaluator) = 
struct
  include Lattice.Prod3(CPA)(Flag)(VarMap)

  let join ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    (CPA.smart_join a1 a2 (ExpEval.eval_exp one) (ExpEval.eval_exp two), Flag.join b1 b2, VarMap.join c1 c2)

  let leq ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    CPA.smart_leq a1 a2 (ExpEval.eval_exp one) (ExpEval.eval_exp two) && Flag.leq b1 b2 && VarMap.leq c1 c2

  let widen ((a1, b1, c1) as one) ((a2, b2, c2) as two) =
    (CPA.smart_widen a1 a2 (ExpEval.eval_exp one) (ExpEval.eval_exp two), Flag.widen b1 b2, VarMap.widen c1 c2)
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval = DomFunctor(struct
  module M = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let eval_exp x e =
    let (x, _, _) = x in
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match M.find v x with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None 
end)