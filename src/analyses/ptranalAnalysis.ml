(** CIL's {!GoblintCil.Ptranal} for function pointer evaluation ([ptranal]).

    Useful for sound analysis of function pointers without base. *)

open GoblintCil
open Analyses

module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "ptranal"

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalFunvar (Lval (Mem e, _)) ->
      let funs = Ptranal.resolve_exp e in
      List.fold_left (fun xs f -> Queries.AD.add (Queries.AD.Addr.of_var f) xs) (Queries.AD.empty ()) funs
    | _ -> Queries.Result.top q

  let init _: unit =
    Ptranal.analyze_file !Cilfacade.current_file;
    Ptranal.compute_results false

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
