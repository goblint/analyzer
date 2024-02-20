(** 
   The purpose of this analysis is to track which variables have their address taken. This used in [relationAnalysis.apron] when pointert_tracking is enabled. 
   This analysis is path sensitive. Pointer_tracking is therefore not supported for multi-threaded programs.
*)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "addressOfPointer"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars


  let rec pointerAddressTakenOf acc e = match e with 
    (*AddrOf (Mem e) should be already simplified*)
    | AddrOf (Var v, _) -> D.add (RelationAnalysis.PointerMap.to_varinfo ~isGlobal:v.vglob v) acc
    (* 0 + (int) &p *)
    | BinOp (_, e1, e2, _) -> pointerAddressTakenOf (pointerAddressTakenOf acc e1) e2 
    | CastE (_, e) -> pointerAddressTakenOf acc e
    | Question (_, e2, e3, _) -> pointerAddressTakenOf (pointerAddressTakenOf acc e2) e3
    | e -> acc

  let assign ctx (lval:lval) (rval:exp) = 
    pointerAddressTakenOf ctx.local rval

  let combine_env ctx lval fexp f args fc au f_ask =
    let au_filtered = D.filter (fun x -> x.vglob) au in
    D.join au_filtered ctx.local

  let enter ctx r f (args:exp list) = 
    (*track all arguments we take the address of*)
    let ctx_new = List.fold_left pointerAddressTakenOf ctx.local args in
    [ctx.local, ctx_new]

  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | Queries.AddressOfPointerTaken v -> 
      D.mem v ctx.local
    | _ -> Queries.Result.top q


  let startstate v = D.bot ()

  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
