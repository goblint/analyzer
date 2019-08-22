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
  
  let eval x e =
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match find v x with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None 

  let smart_join (x:t) (y:t) =
    let normal_join = join x y in
    if is_top normal_join || is_bot normal_join then
      normal_join
    else
      if x == y then x else long_map2 (fun (a:VD.t) (b:VD.t) -> VD.smart_join a b (eval x) (eval y)) x y

  let smart_widen (x:t) (y:t) =
    let normal_widen = widen x y in
    if is_top normal_widen || is_bot normal_widen then
      normal_widen
    else
      if x == y then x else long_map2 (fun (a:VD.t) (b:VD.t) -> VD.smart_widen a b (eval x) (eval y)) x y


  let smart_leq (x:t) (y:t) =
    if is_top y then true
    else if is_bot x then true
    else if is_bot y then is_bot x
    else if is_top x then is_top y (* at this point, we have dealt with all _|_ and T *)
    else
    (* For each key-value in m1, the same key must be in m2 with a geq value: *)
      let p key value =
        try VD.smart_leq value (find key y) (eval x) (eval y) with Not_found -> false
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

module Dom = 
struct
  include Lattice.Prod3(CPA)(Flag)(VarMap)

  let join (a1,b1,c1) (a2,b2,c2) =
    (CPA.smart_join a1 a2, Flag.join b1 b2, VarMap.join c1 c2)

  let leq (a1, b1, c1) (a2, b2, c2) =
    CPA.smart_leq a1 a2 && Flag.leq b1 b2 && VarMap.leq c1 c2

  let widen (a1,b1,c1) (a2,b2,c2) =
    (CPA.smart_widen a1 a2, Flag.widen b1 b2, VarMap.widen c1 c2)
end