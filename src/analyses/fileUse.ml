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
    (* let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    let fo, fc = ctx.local in (fo, fc)
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    (* Messages.report ("return: ctx.local="^(Dom.short 50 ctx.local)); *)
    let loc = !Tracing.current_loc in
    let fo, fc = ctx.local in
    let diff = Dom.VarSet.diff fo fc in
    if not (Dom.VarSet.is_empty diff) then (let vars = String.concat ", " (List.map (fun v -> v.vname) (Dom.VarSet.elements diff)) in
      Messages.print_group "file" ["unclosed files: "^vars, loc]);
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
    let loc = !Tracing.current_loc in
    let fo, fc = ctx.local in
    let dummy = [ctx.local, Cil.integer 1, true] in
    match f.vname with
      | "fopen" -> begin (* Messages.report "special_fn: found fopen"; *)
          match lval with
            | None -> Messages.print_group "file" ["file handle is not saved!", loc]; dummy
            | Some (lhost,offset) ->
                match lhost with
                  | Var varinfo -> (* Messages.report ("file handle saved in variable "^varinfo.vname); *)
                      [(Dom.VarSet.add varinfo fo, fc), Cil.integer 1, true]
                  | Mem exp -> Messages.report "TODO: save to object in memory"; dummy
          end
      | "fclose" -> begin (* Messages.report "special_fn: found fclose"; *)
          match arglist with
            | [fp] -> begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo -> (* Messages.report ("closing file handle "^varinfo.vname); *)
                          if not (Dom.VarSet.mem varinfo fo) then (Messages.print_group "file" ["closed unopened file handle "^varinfo.vname, loc]);
                          [(fo, Dom.VarSet.add varinfo fc), Cil.integer 1, true]
                      | Mem exp -> dummy
                    end
                | _ -> dummy (* TODO: only considers variables as arguments *)
              end
            | _ -> M.bailwith "fclose needs exactly one argument"
          end
      | "fprintf" -> begin (* Messages.report ("fprintf: ctx.local="^(Dom.short 50 ctx.local)); *)
          match arglist with
            | fp::xs -> begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo -> Messages.report ("printf to file handle "^varinfo.vname);
                          if not (Dom.VarSet.mem varinfo fo) then (Messages.print_group "file" ["writing to unopened file handle "^varinfo.vname, loc]);
                          if     (Dom.VarSet.mem varinfo fc) then (Messages.print_group "file" ["writing to closed file handle "^varinfo.vname, loc]);
                          dummy
                      | Mem exp -> dummy
                    end
                | _ -> (* let _ = List.iter (fun exp -> ignore(printf "%a\n" (printExp plainCilPrinter) exp)) arglist in *)
                       let _ = List.iter (fun exp -> Messages.report ("vname: "^(fst exp).vname)) (query_lv ctx.ask fp) in
                       Messages.report ("printf not Lval"); dummy
              end
            | _ -> M.bailwith "fprintf needs at least two arguments"
          end
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
