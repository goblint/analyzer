(** An analysis for checking correct use of file handles. *)

open Cil
open Pretty
open Analyses
open Batteries

module M = Messages


module Spec =
struct
  include Analyses.DefaultSpec

  let name = "file"
  module D = FileDomain.Dom
  module C = FileDomain.Dom
  module G = Lattice.Unit

  (* special variables *)
  let return_var    = Cil.makeVarinfo false "@return"    Cil.voidType, `NoOffset
  let unclosed_var  = Cil.makeVarinfo false "@unclosed"  Cil.voidType, `NoOffset

  (* keys that were already warned about; needed for multiple returns (i.e. can't be kept in D) *)
  let warned_unclosed = ref Set.empty

  (* one Lval may yield multiple keys *)
  let key_from_lval = function
    | Var varinfo, offset -> varinfo, Lval.CilLval.of_ciloffs offset
    | Mem exp, offset -> failwith "not implemented yet" (* TODO use query_lv *)

  (* get string from Cil values, e.g. sprint d_exp exp, sprint d_plainlval lval etc. *)
  let sprint f x = Pretty.sprint 80 (f () x)

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      | Queries.MayPointTo exp -> M.debug_each @@ "query MayPointTo: "^sprint d_plainexp exp; Queries.Result.top ()
      | _ -> Queries.Result.top ()

  let query_lv ask exp =
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) ->
          Queries.LS.elements l
      | _ -> []

  let rec eval_fv ask exp: varinfo option =
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

  let print_query_lv ?msg:(msg="") ask exp =
    let xs = query_lv ask exp in (* MayPointTo -> LValSet *)
    M.debug_each @@ msg^" MayPointTo "^sprint d_exp exp^" = ["
      ^String.concat ", " (List.map D.string_of_key xs)^"]"


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let m = ctx.local in
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let saveOpened ?unknown:(unknown=false) k m = (* save maybe opened files in the domain to warn about maybe unclosed files at the end *)
      if D.may k D.opened m && not (D.is_unknown k m) then (* if unknown we don't have any location for the warning and have handled it already anyway *)
        let mustOpen, mayOpen = D.filter_records k D.opened m in
        let mustOpen, mayOpen = if unknown then Set.empty, mayOpen else mustOpen, Set.diff mayOpen mustOpen in
        D.extend_value unclosed_var (mustOpen, mayOpen) m
      else m
    in
    let key_from_exp = function
      | Lval x -> Some (key_from_lval x)
      | _ -> None
    in
    match key_from_exp (Lval lval), key_from_exp rval with (* we just care about Lval assignments *)
      | Some k1, Some k2 when k1=k2 -> m (* do nothing on self-assignment *)
      | Some k1, Some k2 when D.mem k1 m && D.mem k2 m -> (* both in D *)
          saveOpened k1 m |> D.remove' k1 |> D.alias k1 k2
      | Some k1, Some k2 when D.mem k1 m -> (* only k1 in D *)
          saveOpened k1 m |> D.remove' k1
      | Some k1, Some k2 when D.mem k2 m -> (* only k2 in D *)
          D.alias k1 k2 m
      | Some k1, _ when D.mem k1 m -> (* k1 in D and assign something unknown *)
          D.warn @@ "changed file pointer "^D.string_of_key k1^" (no longer safe)";
          saveOpened ~unknown:true k1 m |> D.unknown k1
      | _ -> m (* no change in D for other things *)

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let m = ctx.local in
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    let check a b tv =
      (* ignore(printf "check: %a = %a, %B\n" d_plainexp a d_plainexp b tv); *)
      match a, b with
      | Const (CInt64(i, kind, str)), Lval lval
      | Lval lval, Const (CInt64(i, kind, str)) ->
        (* ignore(printf "branch(%s==%i, %B)\n" v.vname (Int64.to_int i) tv); *)
        let k = key_from_lval lval in
        if i = Int64.zero && tv then (
          (* ignore(printf "error-branch\n"); *)
          D.error k m
        )else
          D.success k m
      | _ -> M.debug @@ "nothing matched the given BinOp: "^sprint d_plainexp a^" = "^sprint d_plainexp b; m
    in
    match stripCasts (constFold true exp) with
      (* somehow there are a lot of casts inside the BinOp which stripCasts only removes when called on the subparts
      -> matching as in flagMode didn't work *)
(*     | BinOp (Eq, Const (CInt64(i, kind, str)), Lval (Var v, NoOffset), _)
    | BinOp (Eq, Lval (Var v, NoOffset), Const (CInt64(i, kind, str)), _) ->
        ignore(printf "%s %i\n" v.vname (Int64.to_int i)); m *)
    | BinOp (Eq, a, b, _) -> check (stripCasts a) (stripCasts b) tv
    | BinOp (Ne, a, b, _) -> check (stripCasts a) (stripCasts b) (not tv)
    | e -> M.debug @@ "branch: nothing matched the given exp: "^sprint d_plainexp e; m

  let body ctx (f:fundec) : D.t =
    (* M.debug_each @@ "body of function "^f.svar.vname; *)
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* TODO check One Return transformation: oneret.ml *)
    let m = ctx.local in
    (* M.debug_each @@ "return: ctx.local="^D.short 50 ctx.local^string_of_callstack m; *)
    (* if f.svar.vname <> "main" && BatList.is_empty (callstack m) then M.write ("\n\t!!! call stack is empty for function "^f.svar.vname^" !!!"); *)
    if f.svar.vname = "main" then (
      let mustOpen, mayOpen = D.union (D.filter_values D.opened m) (D.get_value unclosed_var m) in
      if Set.cardinal mustOpen > 0 then (
        D.warn @@ "unclosed files: "^D.string_of_keys mustOpen;
        Set.iter (fun v -> D.warn ~loc:(D.V.loc v) "file is never closed") mustOpen;
        (* add warnings about currently open files (don't include overwritten or changed file handles!) *)
        warned_unclosed := Set.union !warned_unclosed (fst (D.filter_values D.opened m)) (* can't save in domain b/c it wouldn't reach the other return *)
      );
      (* go through files "never closed" and recheck for current return *)
      Set.iter (fun v -> if D.must (D.V.key v) D.closed m then D.warn ~may:true ~loc:(D.V.loc v) "file is never closed") !warned_unclosed;
      (* let mustOpenVars = List.map (fun x -> x.key) mustOpen in *)
      (* let mayOpen = List.filter (fun x -> not (List.mem x.key mustOpenVars)) mayOpen in (* ignore values that are already in mustOpen *) *)
      let mayOpen = Set.diff mayOpen mustOpen in
      if Set.cardinal mayOpen > 0 then
        D.warn ~may:true @@ "unclosed files: "^D.string_of_keys mayOpen;
        Set.iter (fun v -> D.warn ~may:true ~loc:(D.V.loc v) "file is never closed") mayOpen
    );
    let au = match exp with
      | Some(Lval lval) when D.mem (key_from_lval lval) m -> (* we return a var in D *)
          let k = key_from_lval lval in
          let varinfo,offset = k in
          if List.mem varinfo (f.sformals @ f.slocals) then (* if var is local, we make a copy *)
            D.add return_var (D.find' k m) m
          else
            D.alias return_var k m (* if var is global, we alias it *)
      | _ -> m
    in
    (* remove formals and locals *)
    List.fold_left (fun m var -> D.remove' (var, `NoOffset) m) au (f.sformals @ f.slocals)

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (* M.debug_each @@ "entering function "^f.vname^string_of_callstack ctx.local; *)
    let m = if f.vname <> "main" then
      D.edit_callstack (BatList.cons !Tracing.current_loc) ctx.local
    else ctx.local in [m,m]

  let check_overwrite_open k m = (* used in combine and special *)
    if List.is_empty (D.get_aliases k m) then (
      (* there are no other variables pointing to the file handle
         and it is opened again without being closed before *)
      D.report k D.opened ("overwriting still opened file handle "^D.string_of_key k) m;
      let mustOpen, mayOpen = D.filter_records k D.opened m in
      let mayOpen = Set.diff mayOpen mustOpen in
      (* save opened files in the domain to warn about unclosed files at the end *)
      D.extend_value unclosed_var (mustOpen, mayOpen) m
    ) else m

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    (* M.debug_each @@ "leaving function "^f.vname^string_of_callstack au; *)
    let au = D.edit_callstack List.tl au in
    let return_val = D.find_option return_var au in
    match lval, return_val with
    | Some lval, Some v ->
        let k = key_from_lval lval in
        (* remove special return var and handle potential overwrites *)
        let au = D.remove' return_var au |> check_overwrite_open k in
        (* if v.key is still in D, then it must be a global and we need to alias instead of rebind *)
        (* TODO what if there is a local with the same name as the global? *)
        if D.V.is_top v then (* returned a local that was top -> just add k as top *)
          D.add' k v au
        else (* v is now a local which is not top or a global which is aliased *)
          let vvar = D.V.get_alias v in (* this is also ok if v is not an alias since it chooses an element from the May-Set which is never empty (global top gets aliased) *)
          if D.mem vvar au then (* returned variable was a global TODO what if local had the same name? -> seems to work *)
            (* let _ = M.debug @@ vvar.vname^" was a global -> alias" in *)
            D.alias k vvar au
          else (* returned variable was a local *)
            let v = D.V.set_key k v in (* ajust var-field to lval *)
            (* M.debug @@ vvar.vname^" was a local -> rebind"; *)
            D.add' k v au
    | _ -> au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let m = ctx.local in
    let ret_branch_err lval dom =
      (* type? NULL = 0 = 0-ptr? Cil.intType, Cil.intPtrType, Cil.voidPtrType -> no difference *)
      (* let f tv = dom, Cil.BinOp (Cil.Eq, Cil.Lval lval, Cil.mkCast (Cil.integer 0) Cil.intPtrType, Cil.intType), tv *)
      if not (GobConfig.get_bool "ana.file.optimistic") then
        ctx.split dom (Cil.BinOp (Cil.Eq, Cil.Lval lval, Cil.integer 0, Cil.intType)) true;
      dom
    in
    let loc = !Tracing.current_loc::(D.callstack m) in
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    let keys_from_lval lval = (* get possible keys for a given lval *)
      match lval with
        | Var varinfo, offset -> [varinfo, Lval.CilLval.of_ciloffs offset]
        | Mem lval2, offset -> (* e.g. \*fp -> Mem(Lval(Var(fp, offset))), \*(fp+1) -> Mem(PlusPI(...)) *)
            let exp = lval2 in
            let xs = query_lv ctx.ask exp in (* MayPointTo -> LValSet *)
            M.debug_each @@ "MayPointTo "^sprint d_exp exp^" = ["
              ^String.concat ", " (List.map D.string_of_key xs)^"]";
            List.map (fun (v,o) -> let new_ciloffs = addOffset (Lval.CilLval.to_ciloffs o) offset in
              v, Lval.CilLval.of_ciloffs new_ciloffs) xs
    in
    (* fold possible keys on domain *)
    let ret_all ?ret:(retf=identity) f lval =
      let xs = keys_from_lval lval in
      if List.length xs = 1 then retf (f (List.hd xs) m)
      (* if there are more than one, each one will be May, TODO: all together are Must *)
      else retf (List.fold_left (fun m k -> f k m |> D.unknown k) m xs) in (* TODO replaced may with top -> fix *)
    match lval, f.vname, arglist with
      | None, "fopen", _ ->
          D.warn "file handle is not saved!"; m
      | Some lval, "fopen", _ ->
          let f k m =
            let m = check_overwrite_open k m in
            (match arglist with
              | Const(CStr(filename))::Const(CStr(mode))::[] ->
                  (* M.debug_each @@ "fopen(\""^filename^"\", \""^mode^"\")"; *)
                  D.fopen k loc filename mode m
              | e::Const(CStr(mode))::[] ->
                  (* ignore(printf "CIL: %a\n" d_plainexp e); *)
                  (match ctx.ask (Queries.EvalStr e) with
                    | `Str filename -> D.fopen k loc filename mode m
                    | _ -> D.warn "unknown filename"; D.fopen k loc "???" mode m
                  )
              | xs ->
                  let args = (String.concat ", " (List.map (sprint d_exp) xs)) in
                  M.debug @@ "fopen args: "^args;
                  (* List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) xs; *)
                  D.warn @@ "fopen needs two strings as arguments, given: "^args; m
            )
          in ret_all ~ret:(ret_branch_err lval) f lval

      | _, "fclose", [Lval fp] ->
          let f k m =
            D.reports k [
              false, D.closed,  "closeing already closed file handle "^D.string_of_key k;
              true,  D.opened,  "closeing unopened file handle "^D.string_of_key k
            ] m;
            D.fclose k loc m
          in ret_all f fp
      | _, "fclose", _ ->
          D.warn "fclose needs exactly one argument"; m

      | _, "fprintf", (Lval fp)::_::_ ->
          let f k m =
            D.reports k [
              false, D.closed,   "writing to closed file handle "^D.string_of_key k;
              true,  D.opened,   "writing to unopened file handle "^D.string_of_key k;
              true,  D.writable, "writing to read-only file handle "^D.string_of_key k;
            ] m;
            m
          in ret_all f fp
      | _, "fprintf", fp::_::_ ->
          (* List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) arglist; *)
          print_query_lv ~msg:"fprintf(?, ...): " ctx.ask fp;
          D.warn "first argument to printf must be a Lval"; m
      | _, "fprintf", _ ->
          D.warn "fprintf needs at least two arguments"; m

      | _ -> m

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
