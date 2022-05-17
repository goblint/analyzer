open Pretty
open Cil

module Exp =
struct
  include CilType.Exp

  let name () = "Cil expressions"

  let  contains_var v e =
    let rec offs_contains o =
      match o with
      | NoOffset -> false
      | Field (_,o) -> offs_contains o
      | Index (e,o) -> cv false e || offs_contains o
    and cv deref e =
      match e with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | Const _
      | AlignOfE _ -> false
      | UnOp  (_,e,_)     -> cv deref e
      | BinOp (_,e1,e2,_) -> cv deref e1 || cv deref e2
      | AddrOf  (Mem e,o)
      | StartOf (Mem e,o)
      | Lval    (Mem e,o) -> cv true e || offs_contains o
      | CastE (_,e)           -> cv deref e
      | Lval    (Var v2,o) -> CilType.Varinfo.equal v v2 || offs_contains o
      | AddrOf  (Var v2,o)
      | StartOf (Var v2,o) ->
        if deref
        then CilType.Varinfo.equal v v2 || offs_contains o
        else offs_contains o
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    cv false e
end
