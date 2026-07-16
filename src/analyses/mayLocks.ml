(** May lockset analysis and analysis of double locking ([maylocks]). *)

open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg:LocksetAnalysis.MayArg =
struct
  module D = LockDomain.MayLocksetNoRW
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add man (l,r) =
    if D.mem l man.local then
      let default () =
        M.warn ~category:M.Category.Behavior.Undefined.double_locking "Acquiring a (possibly non-recursive) mutex that may be already held";
        man.local
      in
      match D.Addr.to_mval l with
      | Some (v,o) ->
        (let mtype = man.ask (Queries.MutexType (v, Offset.Unit.of_offs o)) in
         match mtype with
         | `Lifted MutexAttrDomain.MutexKind.Recursive -> man.local
         | `Lifted MutexAttrDomain.MutexKind.NonRec ->
           M.warn ~category:M.Category.Behavior.Undefined.double_locking "Acquiring a non-recursive mutex that may be already held";
           man.local
         | _  -> default ())
      | _ -> default ()
    else
      D.add l man.local

  let remove man l =
    if not (D.mem l man.local) then M.error "Releasing a mutex that is definitely not held";
    match D.Addr.to_mval l with
    | Some (v,o) ->
      (let mtype = man.ask (Queries.MutexType (v, Offset.Unit.of_offs o)) in
       match mtype with
       | `Lifted MutexAttrDomain.MutexKind.NonRec -> D.remove l man.local
       | _ -> man.local (* we cannot remove them here *))
    | None -> man.local (* we cannot remove them here *)
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocks"

  let exitstate  v = D.top () (* TODO: why? *)

  let return man exp fundec =
    if not (D.is_bot man.local) && ThreadReturn.is_current (Analyses.ask_of_man man) then M.warn "Exiting thread while still holding a mutex!";
    man.local

  let special man (lv:lval option) (f: varinfo) (args: exp list) =
    (match(LF.find f).special args with
     | ThreadExit _ -> if not @@ D.is_bot man.local then M.warn "Exiting thread while still holding a mutex!"
     | _ -> ())
    ;
    man.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
