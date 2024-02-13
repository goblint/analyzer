(*** Alloc ghost variable analysis
     tracks all alloc ghost variables that get assigned to a global variable *)

open GoblintCil
open Analyses

module M = Messages
module AD = Queries.AD


module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "allocVarEscaped"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars


  let mpt ctx (ask: Queries.ask) e: D.t =
    match ask.f (Queries.MayPointTo e) with
    | ad when not (AD.is_top ad) ->
      let to_extra addr set =
        match addr with
        | AD.Addr.Addr (v,_) -> D.add (RelationAnalysis.AllocSize.to_varinfo ~isGlobal:true v) set
        | _ -> set
      in
      AD.fold to_extra (AD.remove UnknownPtr ad) (D.empty ())
    | ad ->
      if M.tracing then M.tracel "escape" "mpt %a: %a\n" d_exp e AD.pretty ad;
      ctx.local

  let assign ctx (lval:lval) (rval:exp) = 
    match lval with 
    | (Var v, _) -> if v.vglob then 
        let ask = Analyses.ask_of_ctx ctx in
        mpt ctx ask rval 
      else 
        ctx.local
    | _ ->  
      ctx.local


  let combine_assign ctx (lval:lval option) (fexp:exp) f args fc au f_ask : D.t =
    (* let ask = Analyses.ask_of_ctx ctx in *)
    match lval with
    | Some lval  -> 
      begin     match lval  with 
        | (Var v, _) -> if v.vglob then 
            let ask = Analyses.ask_of_ctx ctx in
            mpt ctx ask fexp 
          else 
            ctx.local
        | _ ->  
          ctx.local
      end

    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | Queries.AllocAssignedToGlobal v -> D.mem v ctx.local
    | _ -> Queries.Result.top q


  let startstate v = D.bot ()

  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
