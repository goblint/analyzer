(** May lockset analysis and analysis of double locking. *)
open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg:LocksetAnalysis.MayArg =
struct
  module D = LockDomain.MayLocksetNoRW
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add ctx (l,r) =
    if D.mem l ctx.local then
      let default () =
        M.warn ~category:M.Category.Behavior.Undefined.double_locking "Acquiring a (possibly non-recursive) mutex that may be already held";
        ctx.local
      in
      match D.Addr.to_var_must l with
      | Some v ->
        (let mtype = ctx.ask (Queries.MutexType v) in
         match mtype with
         | `Lifted MutexAttrDomain.MutexKind.Recursive -> ctx.local
         | `Lifted MutexAttrDomain.MutexKind.NonRec ->
           M.warn ~category:M.Category.Behavior.Undefined.double_locking "Acquiring a non-recursive mutex that may be already held";
           ctx.local
         | _  -> default ())
      | _ -> default ()
    else
      D.add l ctx.local

  let remove ctx l =
    if not (D.mem l ctx.local) then M.warn "Releasing a mutex that is definitely not held";
    match D.Addr.to_var_must l with
    | Some v ->
      (let mtype = ctx.ask (Queries.MutexType v) in
       match mtype with
       | `Lifted MutexAttrDomain.MutexKind.NonRec -> D.remove l ctx.local
       | _ -> ctx.local (* we cannot remove them here *))
    | None -> ctx.local (* we cannot remove them here *)
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocks"

  let exitstate  v = D.top () (* TODO: why? *)

  let return ctx exp fundec =
    if not (D.is_bot ctx.local) && ThreadReturn.is_current (Analyses.ask_of_ctx ctx) then M.warn "Exiting thread while still holding a mutex!";
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
