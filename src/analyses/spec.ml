open Cil
open Pretty
open Analyses

module M = Messages


module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Spec"
  module Dom  = SpecDomain.Dom
  open Dom.V.T
  module Glob = Glob.Make (Lattice.Unit)

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let return_var = Cil.makeVarinfo false "@return" Cil.voidType
  let stack_var = Cil.makeVarinfo false "@stack" Cil.voidType

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      (* | Queries.MayEscape v -> `Bool (Dom.mem v ctx.local) *)
      | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let m = ctx.local in
    match lval with
      | Var var, offset when Dom.mem var m -> (* var in domain is modified *)
(*           (match rval, ctx.ask (Queries.MayPointTo rval) with (* assignment from other var in domain? *)
            | Lval(Var varinfo, _), _ when Dom.mem varinfo m ->
                M.write ("Lval: Assigned other file handle "^var.vname^" = "^varinfo.vname);
                Dom.addMay var (Dom.find varinfo m) (Dom.may var m)
            | _, `LvalSet a when not (Queries.LS.is_top a) && Queries.LS.cardinal a = 1
              && Dom.mem (fst (Queries.LS.choose a)) m ->
                let varinfo = fst (Queries.LS.choose a) in
                M.write ("Query: Assigned other file handle "^var.vname^" = "^varinfo.vname);
                Dom.addMay var (Dom.find varinfo m) (Dom.may var m)
            | _ -> M.report ("changed file pointer "^var.vname^" (no longer safe)");
                   Dom.may var m
          ) *)
          M.report ("changed file pointer "^var.vname^" (no longer safe)");
          Dom.may var m
      | _ -> m

  let branch ctx (exp:exp) (tv:bool) : Dom.t =
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    ctx.local

  let body ctx (f:fundec) : Dom.t =
    ctx.local

  let callStack m = match Dom.findOption stack_var m with
      | Some(Must(v)) -> v.loc
      | _ -> []

  let callStackStr m = " [call stack: "^(String.concat ", " (List.map (fun x -> string_of_int x.line) (callStack m)))^"]"

  let return ctx (exp:exp option) (f:fundec) : Dom.t =
    let m = ctx.local in
    (* M.write ("return: ctx.local="^(Dom.short 50 ctx.local)^(callStackStr m)); *)
    (* if f.svar.vname <> "main" && BatList.is_empty (callStack m) then M.write ("\n\t!!! call stack is empty for function "^f.svar.vname^" !!!"); *)
    if f.svar.vname = "main" then (
      let vnames xs = String.concat ", " (List.map (fun v -> v.var.vname) xs) in
      let mustOpen = Dom.filterValues Dom.V.opened m in
      if List.length mustOpen > 0 then
        M.report ("unclosed files: "^(vnames mustOpen));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "file is never closed") mustOpen;
      let mustOpenVars = List.map (fun x -> x.var) mustOpen in
      let mayOpenAll = Dom.filterValues ~may:true Dom.V.opened m in
      let mayOpen = List.filter (fun x -> not (List.mem x.var mustOpenVars)) mayOpenAll in (* ignore values that are already in mustOpen *)
      if List.length mayOpen > 0 then
        M.report ("maybe unclosed files: "^(vnames (BatList.unique ~eq:(fun a b -> a.var.vname=b.var.vname) mayOpen)));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "file may be never closed") mayOpen
    );
    let au = match exp with
      | Some(Lval(Var(varinfo),offset)) ->
          (* M.write ("return variable "^varinfo.vname^" (dummy: "^return_var.vname^")"); *)
          Dom.add return_var (Dom.find varinfo m) m
      | _ -> m
    in
    (* remove formals and locals *)
    List.fold_left (fun m var -> Dom.remove var m) au (f.sformals @ f.slocals)

  let editStack f m =
    let v = match Dom.findOption stack_var m with
      | Some(Must(v)) -> {v with loc=(f v.loc)}
      | _ -> Dom.V.create stack_var (f []) Dom.V.Close in
    Dom.add stack_var (Must v) m

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    (* M.write ("entering function "^f.vname^(callStackStr m)); *)
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else ctx.local in [m,m]

  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    (* M.write ("leaving function "^f.vname^(callStackStr au)); *)
    let au = editStack List.tl au in
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some (Var var, offset), Some rval ->
          (* M.write ("setting "^var.vname^" to content of "^(Dom.V.vnames rval)); *)
          let rval = Dom.V.rebind rval var in (* change rval.var to lval *)
          Dom.add var rval (Dom.remove return_var au)
      | _ -> au

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
    let m = ctx.local in
    let ret dom = [dom, Cil.integer 1, true] in
    let dummy = ret ctx.local in
    let loc = !Tracing.current_loc in
    let dloc = loc::(callStack m) in
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    let varinfos lval = (* get possible varinfos for a given lval *)
      match lval with (* TODO ignore offset? *)
        | Var varinfo, _ -> [varinfo]
        | Mem exp, _ ->
            let xs = query_lv ctx.ask exp in (* MayPointTo -> LValSet *)
            M.report ("MayPointTo "^(Pretty.sprint 80 (d_exp () exp))^" = ["
              ^(String.concat ", " (List.map (Lval.CilLval.short 80) xs))^"]");
            List.map fst xs
    in
    (* fold possible varinfos on domain *)
    let ret_all f lval = ret (List.fold_left f m (varinfos lval)) in
    match lval, f.vname, arglist with
      | None, "fopen", _ ->
          M.report "file handle is not saved!"; dummy
      | Some lval, "fopen", _ ->
          let f m varinfo =
            (* opened again, not closed before *)
            Dom.report varinfo Dom.V.opened ("overwriting still opened file handle "^varinfo.vname) m;
            let mustOpen, mayOpen = Dom.checkMay varinfo Dom.V.opened m in
            if mustOpen || mayOpen then (
              let msg = if mayOpen && not mustOpen then "file may be never closed" else "file is never closed" in
              let xs = Dom.filterRecords varinfo Dom.V.opened m in (* length 1 if mustOpen *)
              List.iter (fun x -> M.report ~loc:(BatList.last x.loc) msg) xs
            );
            (match arglist with
              | Const(CStr(filename))::Const(CStr(mode))::[] ->
                  Dom.fopen varinfo dloc filename mode m
              | e::Const(CStr(mode))::[] ->
                  (* ignore(printf "CIL: %a\n" d_plainexp e); *)
                  (match ctx.ask (Queries.EvalStr e) with
                    | `Str filename -> Dom.fopen varinfo dloc filename mode m
                    | _ -> M.report "no result from query"; m
                  )
              | xs ->
                  M.report (String.concat ", " (List.map (fun x -> Pretty.sprint 80 (d_exp () x)) xs));
                  List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) xs;
                  M.report "fopen needs two strings as arguments"; m
            )
          in ret_all f lval

      | _, "fclose", [Lval fp] ->
          let f m varinfo =
            if not (Dom.mem varinfo m) then M.report ("closeing unopened file handle "^varinfo.vname);
            Dom.report varinfo Dom.V.closed ("closeing already closed file handle "^varinfo.vname) m;
            Dom.fclose varinfo dloc m
          in ret_all f fp
      | _, "fclose", _ ->
          M.report "fclose needs exactly one argument"; dummy

      | _, "fprintf", (Lval fp)::_::_ ->
          let f m varinfo =
            Dom.reports m varinfo [
              false, Dom.V.closed,   "writing to closed file handle "^varinfo.vname;
              true,  Dom.V.opened,   "writing to unopened file handle "^varinfo.vname;
              true,  Dom.V.writable, "writing to read-only file handle "^varinfo.vname;
            ];
            m
          in ret_all f fp
      | _, "fprintf", fp::_::_ ->
          (* List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) arglist; *)
          List.iter (fun exp -> M.report ("vname: "^(fst exp).vname)) (query_lv ctx.ask fp);
          M.report "printf not Lval"; dummy
      | _, "fprintf", _ ->
          M.report "fprintf needs at least two arguments"; dummy

      | _ -> dummy

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.bot ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "spec"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `Spec x
                let extract_l x = match x with `Spec x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
