(** An analysis for checking correct use of file handles. *)

open Cil
open Pretty
open Analyses

module M = Messages


module Spec =
struct
  include Analyses.DefaultSpec

  let name = "file_use"
  module D = FileDomain.FileUses
  module C = FileDomain.FileUses
  module G = Lattice.Unit
  open D.V.T


  let return_var = Cil.makeVarinfo false "@return" Cil.voidType
  let stack_var = Cil.makeVarinfo false "@stack" Cil.voidType

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      (* | Queries.MayEscape v -> `Bool (D.mem v ctx.local) *)
      | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let m = ctx.local in
    match lval with
      | Var var, offset when D.mem var m -> (* var in domain is modified *)
(*           (match rval, ctx.ask (Queries.MayPointTo rval) with (* assignment from other var in domain? *)
            | Lval(Var varinfo, _), _ when D.mem varinfo m ->
                M.write ("Lval: Assigned other file handle "^var.vname^" = "^varinfo.vname);
                D.addMay var (D.find varinfo m) (D.may var m)
            | _, `LvalSet a when not (Queries.LS.is_top a) && Queries.LS.cardinal a = 1
              && D.mem (fst (Queries.LS.choose a)) m ->
                let varinfo = fst (Queries.LS.choose a) in
                M.write ("Query: Assigned other file handle "^var.vname^" = "^varinfo.vname);
                D.addMay var (D.find varinfo m) (D.may var m)
            | _ -> M.report ("changed file pointer "^var.vname^" (no longer safe)");
                   D.may var m
          ) *)
          M.report ("changed file pointer "^var.vname^" (no longer safe)");
          D.may var m
      | _ -> m

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let m = ctx.local in
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    let check a b tv =
      (* ignore(printf "check: %a = %a\n" d_plainexp a d_plainexp b); *)
      match a, b with
      | Const (CInt64(i, kind, str)), Lval (Var v, NoOffset)
      | Lval (Var v, NoOffset), Const (CInt64(i, kind, str)) ->
        (* ignore(printf "branch(%s==%i, %B)\n" v.vname (Int64.to_int i) tv); *)
        if i = Int64.zero && tv then (
          (* ignore(printf "error-branch\n"); *)
          D.remove v m
        )else
          m
      | _ -> ignore(printf "nothing matched the given BinOp: %a = %a\n" d_plainexp a d_plainexp b); m
    in
    match stripCasts (constFold true exp) with
      (* somehow there are a lot of casts inside the BinOp which stripCasts only removes when called on the subparts
      -> matching as in flagMode didn't work *)
(*     | BinOp (Eq, Const (CInt64(i, kind, str)), Lval (Var v, NoOffset), _)
    | BinOp (Eq, Lval (Var v, NoOffset), Const (CInt64(i, kind, str)), _) ->
        ignore(printf "%s %i\n" v.vname (Int64.to_int i)); m *)
    | BinOp (Eq, a, b, _) -> check (stripCasts a) (stripCasts b) tv
    | BinOp (Ne, a, b, _) -> check (stripCasts a) (stripCasts b) (not tv)
    | e -> ignore(printf "nothing matched the given exp (check special_fn):\n%a\n" d_plainexp e); m

  let body ctx (f:fundec) : D.t =
    ctx.local

  let callStack m = match D.findOption stack_var m with
      | Some(Must(v)) -> v.loc
      | _ -> []

  let callStackStr m = " [call stack: "^(String.concat ", " (List.map (fun x -> string_of_int x.line) (callStack m)))^"]"

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let m = ctx.local in
    (* M.write ("return: ctx.local="^(D.short 50 ctx.local)^(callStackStr m)); *)
    (* if f.svar.vname <> "main" && BatList.is_empty (callStack m) then M.write ("\n\t!!! call stack is empty for function "^f.svar.vname^" !!!"); *)
    if f.svar.vname = "main" then (
      let vnames xs = String.concat ", " (List.map (fun v -> v.var.vname) xs) in
      let mustOpen = D.filterValues D.V.opened m in
      if List.length mustOpen > 0 then
        M.report ("unclosed files: "^(vnames mustOpen));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "file is never closed") mustOpen;
      let mustOpenVars = List.map (fun x -> x.var) mustOpen in
      let mayOpenAll = D.filterValues ~may:true D.V.opened m in
      let mayOpen = List.filter (fun x -> not (List.mem x.var mustOpenVars)) mayOpenAll in (* ignore values that are already in mustOpen *)
      if List.length mayOpen > 0 then
        M.report ("MAYBE unclosed files: "^(vnames (BatList.unique ~eq:(fun a b -> a.var.vname=b.var.vname) mayOpen)));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "MAYBE file is never closed") mayOpen
    );
    let au = match exp with
      | Some(Lval(Var(varinfo),offset)) ->
          (* M.write ("return variable "^varinfo.vname^" (dummy: "^return_var.vname^")"); *)
          D.add return_var (D.find varinfo m) m
      | _ -> m
    in
    (* remove formals and locals *)
    List.fold_left (fun m var -> D.remove var m) au (f.sformals @ f.slocals)

  let editStack f m =
    let v = match D.findOption stack_var m with
      | Some(Must(v)) -> {v with loc=(f v.loc)}
      | _ -> D.V.create stack_var (f []) D.V.Close in
    D.add stack_var (Must v) m

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (* M.write ("entering function "^f.vname^(callStackStr m)); *)
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else ctx.local in [m,m]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    (* M.write ("leaving function "^f.vname^(callStackStr au)); *)
    let au = editStack List.tl au in
    let return_val = D.findOption return_var au in
    match lval, return_val with
      | Some (Var var, offset), Some rval ->
          (* M.write ("setting "^var.vname^" to content of "^(D.V.vnames rval)); *)
          let rval = D.V.rebind rval var in (* change rval.var to lval *)
          D.add var rval (D.remove return_var au)
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

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let m = ctx.local in
    (* let ret dom = [dom, Cil.integer 1, true] in *)
    let ret dom = dom in (* XX *)
    let ret_branch_err lval dom =
      (* type? NULL = 0 = 0-ptr? Cil.intType, Cil.intPtrType, Cil.voidPtrType *)
      (* no difference *)
      (* let f tv = dom, Cil.BinOp (Cil.Eq, Cil.Lval lval, Cil.mkCast (Cil.integer 0) Cil.intPtrType, Cil.intType), tv *)
      let f tv = dom, Cil.BinOp (Cil.Eq, Cil.Lval lval, Cil.integer 0, Cil.intType), tv
      (* in [f true; f false] *)
      in dom (* XX *)
    in
    let dummy = ret ctx.local in
    let loc = !Tracing.current_loc in
    let dloc = loc::(callStack m) in
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    let varinfos lval = (* get possible varinfos for a given lval *)
      match lval with (* TODO ignore offset? *)
        | Var varinfo, _ -> [varinfo]
        | Mem exp, _ ->
            let xs = query_lv ctx.ask exp in (* MayPointTo -> LValSet *)
            M.debug_each ("MayPointTo "^(Pretty.sprint 80 (d_exp () exp))^" = ["
              ^(String.concat ", " (List.map (Lval.CilLval.short 80) xs))^"]");
            List.map fst xs
    in
    (* fold possible varinfos on domain *)
    let ret_all ?ret:(retf=ret) f lval =
      let xs = varinfos lval in
      if List.length xs = 1 then ret (f m (List.hd xs))
      (* if there are more than one, each one will be May, TODO: all together are Must *)
      else retf (List.fold_left (fun m v -> D.may v (f m v)) m xs) in
    match lval, f.vname, arglist with
      | None, "fopen", _ ->
          M.report "file handle is not saved!"; dummy
      | Some lval, "fopen", _ -> (* TODO: also return a domain where fp = NULL (error case) *)
          let f m varinfo =
            (* opened again, not closed before *)
            D.report varinfo D.V.opened ("overwriting still opened file handle "^varinfo.vname) m;
            let mustOpen, mayOpen = D.checkMay varinfo D.V.opened m in
            if mustOpen || mayOpen then (
              let msg = if mayOpen && not mustOpen then "MAYBE file is never closed" else "file is never closed" in
              let xs = D.filterRecords varinfo D.V.opened m in (* length 1 if mustOpen *)
              List.iter (fun x -> M.report ~loc:(BatList.last x.loc) msg) xs
            );
            (match arglist with
              | Const(CStr(filename))::Const(CStr(mode))::[] ->
                  D.fopen varinfo dloc filename mode m
              | e::Const(CStr(mode))::[] ->
                  (* ignore(printf "CIL: %a\n" d_plainexp e); *)
                  (match ctx.ask (Queries.EvalStr e) with
                    | `Str filename -> D.fopen varinfo dloc filename mode m
                    | _ -> M.report "no result from query"; m (* TODO open with unknown filename *)
                  )
              | xs ->
                  M.report (String.concat ", " (List.map (fun x -> Pretty.sprint 80 (d_exp () x)) xs));
                  List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) xs;
                  M.report "fopen needs two strings as arguments"; m
            )
          in ret_all ~ret:(ret_branch_err lval) f lval

      | _, "fclose", [Lval fp] ->
          let f m varinfo =
            if not (D.mem varinfo m) then M.report ("closeing unopened file handle "^varinfo.vname);
            D.report varinfo D.V.closed ("closeing already closed file handle "^varinfo.vname) m;
            D.fclose varinfo dloc m
          in ret_all f fp
      | _, "fclose", _ ->
          M.report "fclose needs exactly one argument"; dummy

      | _, "fprintf", (Lval fp)::_::_ ->
          let f m varinfo =
            D.reports m varinfo [
              false, D.V.closed,   "writing to closed file handle "^varinfo.vname;
              true,  D.V.opened,   "writing to unopened file handle "^varinfo.vname;
              true,  D.V.writable, "writing to read-only file handle "^varinfo.vname;
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

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
