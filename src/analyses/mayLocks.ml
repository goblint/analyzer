(** May lockset analysis and analysis of double locking. *)
open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg:LocksetAnalysis.MayArg =
struct
  module D = LockDomain.MayLocksetNoRW
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add ctx (l,_) =
    if D.mem l ctx.local then
      match D.Addr.to_var_must l with
      | Some v when ctx.ask (Queries.IsRecursiveMutex v)->
        ctx.local
      | _ ->
        (M.warn "double locking"; ctx.local)
    else
      D.add l ctx.local

  let remove ctx l = D.remove l ctx.local
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocks"

  let exitstate  v = D.top () (* TODO: why? *)

  let return ctx exp fundec =
    if not @@ D.is_bot ctx.local && ThreadReturn.is_current (Analyses.ask_of_ctx ctx) then M.warn "Exiting thread while still holding a mutex!";
    ctx.local

  let special ctx (lv:lval option) (f: varinfo) (args: exp list) =
    (match(LF.find f).special args with
     | ThreadExit _ -> if not @@ D.is_bot ctx.local then M.warn "Exiting thread while still holding a mutex!"
     | _ -> ())
    ;
    ctx.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
