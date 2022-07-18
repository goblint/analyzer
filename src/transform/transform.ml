open Prelude
open Cil

module type S = sig
  val transform : (Cil.location -> Queries.ask) -> file -> unit (* modifications are done in-place by CIL :( *)
end

let h = Hashtbl.create 13
let register name (module T : S) = Hashtbl.add h name (module T : S)
let run name =
  let module T = (val try Hashtbl.find h name with Not_found -> failwith @@ "Transformation "^name^" does not exist!") in
  if GobConfig.get_bool "dbg.verbose" then print_endline @@ "Starting transformation " ^ name;
  T.transform

module PartialEval = struct
  let loc = ref locUnknown (* when we visit an expression, we need the current location -> store at stmts *)
  class visitor ask = object
    inherit nopCilVisitor
    method! vstmt s =
      loc := Cilfacade.get_stmtLoc s;
      (* ignore @@ Pretty.printf "Set loc at stmt %a to %a\n" d_stmt s CilType.Location.pretty !loc; *)
      DoChildren
    method! vexpr e =
      let eval e = match (ask !loc).Queries.f (Queries.EvalInt e) with
        | x when Queries.ID.is_int x ->
          let i = Option.get @@ Queries.ID.to_int x in
          let e' = integer @@ IntOps.BigIntOps.to_int i in
          ignore @@ Pretty.printf "Replacing non-constant expression %a with %a at %a\n" d_exp e d_exp e' CilType.Location.pretty !loc;
          e'
        | _ ->
          ignore @@ Pretty.printf "Can't replace expression %a at %a\n" d_exp e CilType.Location.pretty !loc; e
      in
      match e with
      | Const _ -> SkipChildren
      | _ -> ChangeDoChildrenPost (e, eval)
  end
  let transform ask file =
    visitCilFile (new visitor ask) file
end
let _ = register "partial" (module PartialEval)
