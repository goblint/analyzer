open Cil
open Pretty
open Analyses

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "File Use"
  module Dom  = FileDomain.FileUses
  module Glob = Glob.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t = 
    match q with
      (* | Queries.MayEscape v -> `Bool (Dom.mem v ctx.local) *)
      | _ -> Queries.Result.top ()
 
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in
    let fo, fc = ctx.local in (fo, fc)
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    Messages.report ("return: ctx.local="^(Dom.short 50 ctx.local));
    ctx.local
    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au

  let rec cut_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (_,o) -> `NoOffset
      | `Field (f,o) -> `Field (f, cut_offset o)
  
  let reachable ask e: Dom.t = 
    match ask (Queries.ReachableFrom e) with
      | `LvalSet a when not (Queries.LS.is_top a) -> Dom.bot ()
           (* let to_extra (v,o) set = Dom.add (Addr.from_var_offset (v, cut_offset o)) set in *)
(*           let to_extra (v,o) set = Dom.add v set in
            Queries.LS.fold to_extra a (Dom.empty ()) *)
      (* Ignore soundness warnings, as invalidation proper will raise them. *)
      | _ -> Dom.bot ()
 
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []

  let rec eval_fv ask (exp:Cil.exp): varinfo option = 
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let fo, fc = ctx.local in
    let dummy = [ctx.local, Cil.integer 1, true] in
    match f.vname with
      | "fopen" -> begin Messages.report "special_fn: found fopen";
          match lval with
            | None -> Messages.report "file handle is not saved..."; dummy
            | Some lval -> let lhost, offset = lval in
                match lhost with
                  | Var varinfo -> Messages.print_group "file" ["file handle saved in variable "^varinfo.vname, varinfo.vdecl];
                      [(Dom.VarSet.add varinfo fo, fc), Cil.integer 1, true] (* TODO: return FILE pointer *)
                  | Mem exp -> Messages.report "TODO: save to object in memory"; dummy
          end
      | "fclose" -> begin Messages.report "special_fn: found fclose";
          match arglist with
            | [fp] -> begin match fp with
                | Lval lval -> begin let lhost, offset = lval in
                    match lhost with
                      | Var varinfo -> Messages.report ("closing file handle "^varinfo.vname);
                          [(fo, Dom.VarSet.add varinfo fc), Cil.integer 1, true]
                      | Mem exp -> dummy
                    end
                | _ -> dummy (* TODO: only considers variables as arguments *)
              end
            | _ -> M.bailwith "fclose needs exactly one argument"
          end
      | "fprintf" -> Messages.print_group "file" ["fprintf: ctx.local="^(Dom.short 50 ctx.local), f.vdecl]; dummy
      | _ -> dummy

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.bot ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "file" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `File x
                let extract_l x = match x with `File x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
