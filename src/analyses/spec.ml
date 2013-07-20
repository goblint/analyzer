(** Analysis by specification file. *)

open Cil
open Pretty
open Analyses
open Batteries

module M = Messages
module SC = SpecCore

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Spec"
  module Dom  = SpecDomain.Dom
  open Dom.V.T
  module Glob = Glob.Make (Lattice.Unit)

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let return_var = Cil.makeVarinfo false "@return" Cil.voidType, `NoOffset
  let stack_var = Cil.makeVarinfo false "@stack" Cil.voidType, `NoOffset
  let global_var = Cil.makeVarinfo false "@global" Cil.voidType, `NoOffset

  let nodes = ref []
  let edges = ref []

  let load_specfile () =
    let specfile = GobConfig.get_string "spec.file" in
    if String.length specfile < 1 then failwith "You need to specify a specification file using --sets spec.file path/to/file.spec when using the spec analysis!";
    if not (Sys.file_exists specfile) then failwith ("The given spec.file ("^specfile^") doesn't exist (CWD is "^Sys.getcwd ()^").");
    let _nodes, _edges = SpecUtil.parseFile specfile in
    nodes := _nodes; edges := _edges (* don't change -> no need to save them in domain *)


  let key_from_lval lval = (* TODO -> keys_from_lval: return list *)
    match lval with
    | Var varinfo, offset -> varinfo, Lval.CilLval.of_ciloffs offset
    | Mem exp, offsett -> failwith "not implemented yet" (* TODO use query_lv *)

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      | _ -> Queries.Result.top ()

  let dump_query_result result =
    match result with
    | `Top -> "`Top"
    | `Int x -> "`Int"
    | `Str x -> "`Str"
    | `Bool x -> "`Bool"
    | `LvalSet x -> "`LvalSet"
    | `ExprSet x -> "`ExprSet"
    | `ExpTriples x -> "`ExpTriples"
    | `Bot -> "`Bot"

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let m = ctx.local in
    m

  (*
  - branch-transitions in the spec-file come in pairs: e.g. true-branch goes to node a, false-branch to node b
  - branch is called for both possibilities
  - TODO query the exp and take/don't take the transition
  - in case of `Top we take the transition
  - both branches get joined after (e.g. for fopen: May [open; error])
  - if there is a branch in the code, branch is also called
    -> get the key from exp and backtrack to the corresponding branch-transitions
    -> reevaluate with current exp and meet domain with result
  *)
  (*
  - get key from exp
  - ask EvalInt
  - if result is `Top and we are in a state that is the starting node of a branch edge, we have to:
    - go to target node and modify the state in specDomain
    - find out which value of key makes exp equal to tv
    - save this value and answer queries for EvalInt with it
  - if not, compare it with tv and take the corresponding branch
  *)
  let branch ctx (exp:exp) (tv:bool) : Dom.t =
    let m = ctx.local in
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    (* try to evaluate the expression using query
      -> if the result is the same as tv, do the corresponding transition, otherwise remove the entry from the domain
      for pointers this won't help since it always returns `Top *)
    (match ctx.ask (Queries.EvalInt exp) with
      | `Int i (* when (Queries.ID.is_bool i) *) -> 
          (match Queries.ID.to_bool i with
          | Some b when b<>tv -> M.debug_each "EvalInt: `Int bool" (* Dom.remove k m TODO where to get the key?? *)
          | _ -> M.debug_each "EvalInt: `Int no bool")
      | `Bool b -> M.debug_each "EvalInt: `Bool"
      | x -> M.debug_each ("OTHER RESULT: "^dump_query_result x)
    );
    let check a b tv =
      (* ignore(printf "check: %a = %a\n" d_plainexp a d_plainexp b); *)
      match a, b with
      | Const (CInt64(i, kind, str)), Lval lval
      | Lval lval, Const (CInt64(i, kind, str)) ->
        (* let binop = BinOp (Eq, a, b, Cil.intType) in *)
        (* standardize the format of the expression to 'lval==i'. -> spec needs to follow that format, the code is mapped to it. *)
        let binop = BinOp (Eq, Lval lval, Const (CInt64(i, kind, str)), Cil.intType) in
        let key = key_from_lval lval in
        let value = Dom.find key m in
        if i = Int64.zero && tv then (
          M.debug_each "error-branch";
          (* Dom.remove key m *)
        )else(
          M.debug_each "success-branch";
          (* m *)
        );
        (* there should always be an entry in our domain for key *)
        if not (Dom.mem key m) then m else
        (* TODO for now we just assume that a Binop is used and Lval is the key *)
        (* get the state(s) that key is/might be in *)
        let states = Dom.get_states key m in
        (* compare SC.exp with Cil.exp and tv *)
        let branch_exp_eq c exp tv =
          (* let c_str = match SC.branch_exp c with Some (exp,tv) -> SC.exp_to_string exp | _ -> "" in
          let c_str = Str.global_replace (Str.regexp_string "$key") "%e:key" c_str in 
          let c_exp = Formatcil.cExp c_str [("key", Fe (Dom.K.to_exp key))] in *)
          (* c_exp=exp *) (* leads to Out_of_memory *)
          match SC.branch_exp c with
          | Some (c_exp,c_tv) ->
            (* let exp_str = Pretty.sprint 80 (d_exp () exp) in *) (* contains too many casts, so that matching fails *)
            let exp_str = Pretty.sprint 80 (d_exp () binop) in
            let c_str = SC.exp_to_string c_exp in
            let c_str = Str.global_replace (Str.regexp_string "$key") (Dom.string_of_key key) c_str in 
            (* ignore(printf "branch_exp_eq: '%s' '%s' -> %B\n" c_str exp_str (c_str=exp_str)); *)
            c_str=exp_str && c_tv=tv
          | _ -> false
        in
        (* filter those edges that are branches, start with a state from states and have the same branch expression and the same tv *)
        let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c && List.mem a states && branch_exp_eq c exp tv) !edges in
        (* there should be only one such edge or none *)
        if List.length branch_edges <> 1 then ( (* call of branch for an actual branch *)
          M.debug_each "branch:branch_edges length is not 1! -> actual branch";
          M.debug_each ((Dom.string_of_entry key m)^" -> branch_edges1: "^(String.concat "\n " @@ List.map (fun x -> SC.def_to_string (SC.Edge x)) branch_edges));
          (* filter those edges that are branches, end with a state from states have the same branch expression and the same tv *)
          (* TODO they should end with any predecessor of the current state, not only the direct predecessor *)
          let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c && List.mem b states && branch_exp_eq c exp tv) !edges in
          M.debug_each ((Dom.string_of_entry key m)^" -> branch_edges2: "^(String.concat "\n " @@ List.map (fun x -> SC.def_to_string (SC.Edge x)) branch_edges));
          if List.length branch_edges <> 1 then m else
          (* meet current value with the target state. this is tricky: we can not simply take the target state, since there might have been more than one element already before the branching.
          -> find out what the alternative branch target was and remove it *)
          let (a,ws,fwd,b,c) = List.hd branch_edges in
          (* the alternative branch has the same start node, the same branch expression and the negated tv *)
          let (a,ws,fwd,b,c) = List.find (fun (a2,ws,fwd,b,c) -> SC.is_branch c && a2=a && branch_exp_eq c exp (not tv)) !edges in
          (* now b is the state the alternative branch goes to -> remove it *)
          (* TODO may etc. *)
          (* being explicit: check how many records there are. if the value is Must b, then we're sure that it is so and we don't remove anything. *)
          if List.length (Dom.V.recordsList value) = 1 then m else
          (* there are multiple possible states -> remove b *)
          let v2 = Dom.V.remove_state value b in
          (* M.write ("branch: changed state from " ^ Dom.V.toString value ^ " to " ^ Dom.V.toString v2); *)
          Dom.add key v2 m
        ) else (* call of branch directly after splitting *)
          let (a,ws,fwd,b,c) = List.hd branch_edges in
          (* TODO may etc. *)
          let v2 = Dom.V.change_state value b in
          (* M.write ("branch: changed state from " ^ Dom.V.toString value ^ " to " ^ Dom.V.toString v2); *)
          Dom.add key v2 m
      | _ -> ignore(printf "nothing matched the given BinOp: %a = %a\n" d_plainexp a d_plainexp b); m
    in
    match stripCasts (constFold true exp) with
      (* somehow there are a lot of casts inside the BinOp which stripCasts only removes when called on the subparts
      -> matching as in flagMode didn't work *)
    | BinOp (Eq, a, b, _) -> check (stripCasts a) (stripCasts b) tv
    | BinOp (Ne, a, b, _) -> check (stripCasts a) (stripCasts b) (not tv)
    | e -> ignore(printf "nothing matched the given exp (check special_fn):\n%a\n" d_plainexp e); m

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
      let warn_main msg_loc msg_end =
        (* find transitions to end state *)
        (* edges that have 'end' as a target *)
        let end_edges = List.filter (fun (a,ws,fwd,b,c) -> b="end") !edges in
        (* we ignore the constraint, TODO maybe find a better syntax for declaring end states *)
        let end_states = List.map (fun (a,ws,fwd,b,c) -> a) end_edges in
        (* used to warn locally or as a summary at the end of main *)
        let warn ?loc:(loc=[!Tracing.current_loc]) maybe msg = M.report ~loc:(List.last loc) ((if maybe then "MAYBE " else "")^msg) in
        (* TODO imperative -> refactor *)
        let must_k = ref [] in
        let may_k = ref [] in
        (* now we check for all entries of our domain if they (maybe) aren't in an end state and warn about it *)
        let check_state k v =
          let must_end = List.exists (fun state -> Dom.in_state k state m) end_states in
          let may_end  = List.exists (fun state -> Dom.may_in_state k state m) end_states in
          let locs = Dom.V.locs v in
          if not may_end then ((* Must: never in an end state *)
            must_k := !must_k@[k];
            match msg_loc with Some msg -> List.iter (fun loc -> warn ~loc:loc false msg) locs | _ -> ()
          )else if not must_end then ((* May: maybe in an end state *)
            may_k := !may_k@[k];
            (* only output MAYBE-warnings for possibilities that are not an end state *)
            (* TODO this is a matter of taste -> make it configurable? *)
            let locs = Dom.V.locs ~p:(fun x -> not (List.mem x.state end_states)) v in
            match msg_loc with Some msg -> List.iter (fun loc -> warn ~loc:loc true msg) locs | _ -> ()
          )
        in
        let no_special_vars = Dom.filter (fun k v -> String.get (Dom.string_of_key k) 0 <> '@') m in
        Dom.iter check_state no_special_vars;
        match msg_end with
        | Some msg ->
          let f msg ks = Str.global_replace (Str.regexp_string "$") (String.concat ", " (List.map Dom.string_of_key ks)) msg in
          if not (List.is_empty !must_k) then warn false (f msg !must_k);
          if not (List.is_empty !may_k) then warn true (f msg !may_k)
        | _ -> ()
      in
      (* check if there is a warning for entries that are not in an end state *)
      match SC.warning "_end" !nodes, SC.warning "_END" !nodes with
      | None, None -> () (* nothing to do here *)
      | msg_loc,msg_end -> warn_main msg_loc msg_end
    );
    let au = match exp with
      | Some(Lval lval) ->
          let k = key_from_lval lval in
          (* M.write ("return variable "^Dom.string_of_key k); *)
          Dom.add return_var (Dom.find k m) m
      | _ -> m
    in
    (* remove formals and locals *)
    List.fold_left (fun m var -> Dom.remove (var, `NoOffset) m) au (f.sformals @ f.slocals)

  let editStack f m =
    let v = match Dom.findOption stack_var m with
      | Some(Must(v)) -> {v with loc=(f v.loc)}
      | _ -> Dom.V.create stack_var (f []) "" in
    Dom.add stack_var (Must v) m

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let m = ctx.local in
    M.debug_each ("entering function "^f.vname^(callStackStr m));
    if f.vname = "main" then load_specfile ();
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else m in [m,m]

  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    (* M.write ("leaving function "^f.vname^(callStackStr au)); *)
    let au = editStack List.tl au in
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some lval, Some rval ->
          let k = key_from_lval lval in
          (* M.write ("setting "^Dom.string_of_key k^" to content of "^(Dom.V.vnames rval)); *)
          let rval = Dom.V.rebind rval k in (* change rval.var to lval *)
          Dom.add k rval (Dom.remove return_var au)
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

(*
.spec-format:
- The file contains two types of definitions: nodes and edges. The labels of nodes are output. The labels of edges are the constraints.
- The given nodes are warnings, which have an implicit back edge to the previous node if used as a target.
- Alternatively warnings can be specified like this: "node1 -w1,w2,w3> node2 ...1" (w1, w2 and w3 will be output when the transition is taken).
- The start node of the first transition is the start node of the automaton.
- End nodes are specified by "node -> end _".
- "_end" is the local warning for nodes that are not in an end state, _END is the warning at return ($ is the list of keys).
- An edge with '_' matches everything.
- Edges with "->>" (or "-w1,w2>>" etc.) are forwarding edges, which will continue matching the same statement for the target node.
*)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    (* let _ = GobConfig.set_bool "dbg.debug" false in *)
    let m = ctx.local in
    let ret dom = [dom, Cil.integer 1, true] in
    let loc = !Tracing.current_loc in
    let dloc = loc::(callStack m) in
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    (* get possible varinfos for a given lval *)
    let varinfos lval = (* TODO merge with key_from_lval *)
      match lval with
        | Var varinfo, offset -> [varinfo, Lval.CilLval.of_ciloffs offset]
        | _ -> (* Mem *)
            let exp = Lval lval in
            let xs = query_lv ctx.ask exp in (* MayPointTo -> LValSet *)
            M.debug_each ("MayPointTo "^(Pretty.sprint 80 (d_exp () exp))^" = ["
              ^(String.concat ", " (List.map (Lval.CilLval.short 80) xs))^"]");
            xs
    in
    (* fold possible varinfos on domain *)
    (* let ret_all f lval = ret (List.fold_left f m (varinfos lval)) in *)
    (* custom goto (Dom.goto is just for modifying) that checks if the target state is a warning and acts accordingly *)
    let goto ?may:(may=false) var loc state m ws =
      let warn var m msg =
        let msg = Str.global_replace (Str.regexp_string "$") (Dom.string_of_key var) msg in
        M.report ((if Dom.is_may var m then "MAYBE " else "")^msg)
      in
      (* do transition warnings *)
      List.iter (fun state -> match SC.warning state !nodes with Some msg -> warn var m msg | _ -> ()) ws;
      match SC.warning state !nodes with
        | Some msg ->
            warn var m msg;
            m (* no goto == implicit back edge *)
        | None ->
            M.debug_each ("GOTO "^Dom.string_of_key var^": "^Dom.string_of_state var m^" -> "^state);
            if may then Dom.may_goto var loc state m else Dom.goto var loc state m
    in
    let matching m new_a old_key (a,ws,fwd,b,c) =
      (* If we have come to a wildcard, we match it instantly, but since there is no way of determining a key
         this only makes sense if fwd is true (TODO wildcard for global. TODO use old_key). We pass a state replacement as 'new_a',
         which will be applied in the following checks.
         Multiple forwarding wildcards are not allowed, i.e. new_a must be None, otherwise we end up in a loop. *)
      if SC.is_wildcard c && fwd && new_a=None then Some (m,fwd,Some (b,a),old_key) (* replace b with a in the following checks *)
      else
      (* Assume new_a  *)
      let a = match new_a with
        | Some (x,y) -> if a=x then y else a
        | None -> a
      in
      (* if we forward, we have to replace the starting state for the following constraints *)
      let new_a = if fwd then Some (b,a) else None in
      (* TODO how to detect the key?? use "$foo" as key, "foo" as var in constraint and "_" for anything we're not interested in.
          What to do for multiple keys (e.g. $foo, $bar)? -> Only allow one key & one map per spec-file (e.g. only $ as a key) or implement multiple maps? *)
      (* look inside the constraint if there is a key and if yes, return what it corresponds to *)
      let key =
        match SC.get_key_variant c with
          | `Lval s    ->
            M.debug_each ("Key variant for "^f.vname^": `Lval "^s^". \027[30m "^SC.stmt_to_string c);
            lval
          | `Arg(s, i) ->
            M.debug_each ("Key variant for "^f.vname^": `Arg("^s^", "^string_of_int i^")"^". "^SC.stmt_to_string c); 
            (try
              let arg = List.at arglist i in
              match arg with
                | Lval x -> Some x (* TODO enough to just assume the arg is already there as a Lval? *)
                | _      -> None
            with Invalid_argument s ->
              M.debug_each ("Key out of bounds! Msg: "^s); (* TODO what to do if spec says that there should be more args... *)
              None
            )
          | _          -> None (* `Rval or `None *)
      in
      (* Now we have the key for the map-domain or we don't.
         In case we don't have a key, we have to check the global state. *)
      let key = match key with
        | Some key -> key
        | None -> Cil.var (fst global_var) (* creates Var with NoOffset *)
      in
      (* ignore(printf "KEY: %a\n" d_plainlval key); *)
      (* possible varinfos the lval may point to -> TODO use Lval as key for map? But: multiple representations for the same Lval?! *)
      let vars = varinfos key in (* does MayPointTo query for Mem, otherwise [varinfo] *)
      let check_var (m,n) var =
        (* M.write ("check_var: " ^ f.vname ^ "(...): " ^ Dom.string_of_entry var m); *)
        (* skip transitions we can't take b/c we're not in the right state *)
        (* i.e. if not in map, we must be at the start node or otherwise we must be in one of the possible saved states *)
        if not (Dom.mem var m) && a<>SC.startnode !edges || Dom.mem var m && not (Dom.may_in_state var a m) then (
          (* ignore(printf "SKIP %s: state: %s, a: %s at %i\n" f.vname (Dom.string_of_state var m) a (!Tracing.current_loc.line)); *)
          (m,n) (* not in map -> initial state. TODO save initial state? *)
        )
        (* skip transitions where the constraint doesn't have the right form (assignment or not) *)
        else if not (SC.equal_form lval c) then (m,n)
        (* check if parameters match those of the constraint (arglist corresponds to args) *)
        else
        let equal_args args =
          if List.length args = 1 && List.hd args = `Free then
            true (* wildcard as an argument matches everything *)
          else if List.length arglist <> List.length args then (
            M.debug_each "SKIP the number of arguments doesn't match the specification!";
            false
          )else
          let equal_arg = function
            (* TODO match constants right away to avoid queries? *)
            | `String a, Const(CStr b) -> M.debug_each ("EQUAL String Const: "^a^" = "^b); a=b
            (* | `String a, Const(CWStr xs as c) -> failwith "not implemented" *)
            (* CWStr is done in base.ml, query only returns `Str if it's safe *)
            | `String a, e -> (match ctx.ask (Queries.EvalStr e) with
                | `Str b -> M.debug_each ("EQUAL String Query: "^a^" = "^b); a=b
                | _      -> M.debug_each "EQUAL String Query: no result!"; false
              )
            | `Regex a, e -> (match ctx.ask (Queries.EvalStr e) with
                | `Str b -> M.debug_each ("EQUAL Regex String Query: "^a^" = "^b); Str.string_match (Str.regexp a) b 0
                | _      -> M.debug_each "EQUAL Regex String Query: no result!"; false
              )
            | `Bool a, e -> (match ctx.ask (Queries.EvalInt e) with
                | `Int b -> (match Queries.ID.to_bool b with Some b -> a=b | None -> false)
                | _      -> M.debug_each "EQUAL Bool Query: no result!"; false
              )
            | `Int a, e  -> (match ctx.ask (Queries.EvalInt e) with
                | `Int b -> (match Queries.ID.to_int b with Some b -> (Int64.of_int a)=b | None -> false)
                | _      -> M.debug_each "EQUAL Int Query: no result!"; false
              )
            | `Float a, Const(CReal (b, fkind, str_opt)) -> a=b
            | `Float a, _ -> M.warn_each "EQUAL Float: unsupported!"; false
            (* arg is a key. currently there can only be one key per constraint, so we already used it for lookup. TODO multiple keys? *)
            | `Vari a, b  -> true
            (* arg is a identifier we use for matching constraints. TODO save in domain *)
            | `Ident a, b -> true
            | `Error s, b -> failwith ("Spec error: "^s)
            (* wildcard matches anything *)
            | `Free, b    -> true
            | a,b -> M.warn_each ("EQUAL? Unmatched case - assume true..."); true
          in List.for_all equal_arg (List.combine args arglist) (* TODO Cil.constFold true arg. Test: Spec and c-file: 1+1 *)
        in
        (* check if arguments match the constraint *)
        if not (equal_args (SC.get_fun_args c)) then (m,n)
        (* everything matches the constraint -> go to new state and increase counter *)
        (* TODO if #Queries.MayPointTo > 1: each result is May, but all combined are Must *)
        else let new_m = goto ~may:(List.length vars > 1) var dloc b m ws in (new_m,n+1)
      in
      (* do check for each varinfo and return the resulting domain if there has been at least one matching constraint *)
      let new_m,n = List.fold_left check_var (m,0) vars in (* start with original domain and #transitions=0 *)
      if n==0 then None (* no constraint matched the current state *)
      else Some (new_m,fwd,new_a,Some key) (* return new domain and forwarding info *)
    in
    (* edges that match the called function name + wildcard transitions, except those for end *)
    let fun_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.fname_is f.vname c || SC.is_wildcard c && fwd && b<>"end") !edges in
    (* go through constraints and return resulting domain for the first match *)
    (* if no constraint matches, the unchanged domain is returned *)
    (* repeat for target node if it is a forwarding edge *)
    (* TODO what should be done if multiple constraints would match? *)
    (* TODO ^^ for May-Sets multiple constraints could match and should be taken! *)
    try
      let rec check_fwd_loop m new_a old_key = (* TODO cycle detection? *)
        let new_m,fwd,new_a,key = List.find_map (matching m new_a old_key) fun_edges in
        (* List.iter (fun x -> M.write (x^"\n")) (Dom.string_of_map new_m); *)
        (* M.write ("fwd: "^dump fwd^", new_a: "^dump new_a^", old_key: "^dump old_key); *)
        if fwd then check_fwd_loop new_m new_a key else new_m,key
      in
      (* now we get the new domain and the latest key that was used *)
      let new_m,key = check_fwd_loop m None None in
      (* List.iter (fun x -> M.write (x^"\n")) (Dom.string_of_map new_m); *)
      (* next we have to check if there is a branch() transition we could take *)
      let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.is_branch c) !edges in
      (* just for the compiler: key is initialized with None, but changes once some constaint matches. If none match, we wouldn't be here but at catch Not_found. *)
      match key with
      | Some key ->
        (* we need to pass the key to the branch function. There is no scheme for getting the key from the constraint, but we should have been forwarded and can use the old key. *)
        let check_branch branches var =
          (* only keep those branch_edges for which our key might be in the right state *)
          let branch_edges = List.filter (fun (a,ws,fwd,b,c) -> Dom.may_in_state var a new_m) branch_edges in
          (* M.write ((Dom.string_of_entry var new_m)^" -> branch_edges: "^(String.concat "\n " @@ List.map (fun x -> SC.def_to_string (SC.Edge x)) branch_edges)); *)
          (* count should be a multiple of 2 (true/false), otherwise the spec is malformed *)
          if List.length branch_edges mod 2 <> 0 then failwith "Spec is malformed: branch-transitions always need a true and a false case!" else
          (* if nothing matches, just return new_m without branching *)
          if List.is_empty branch_edges then Set.of_list (ret new_m) else
          (* unique set of (dom,exp,tv) used in branch *)
          let do_branch branches (a,ws,fwd,b,c) =
            let c_str = match SC.branch_exp c with Some (exp,tv) -> SC.exp_to_string exp | _ -> "" in
            let c_str = Str.global_replace (Str.regexp_string "$key") "%e:key" c_str in (* TODO what should be used to specify the key? *)
            (* TODO this somehow also prints the expression!? why?? *)
            let c_exp = Formatcil.cExp c_str [("key", Fe (Dom.K.to_exp var))] in (* use Fl for Lval instead? *)
            (* TODO encode key in exp somehow *)
            (* ignore(printf "BRANCH %a\n" d_plainexp c_exp); *)
            Set.add (new_m,c_exp,true) (Set.add (new_m,c_exp,false) branches)
          in
          List.fold_left do_branch branches branch_edges
        in
        let vars = varinfos key in
        let new_set = List.fold_left check_branch Set.empty vars in
        List.of_enum (Set.enum new_set)
      | None -> ret new_m
    with Not_found -> ret m (* nothing matched -> no change *)


  let startstate v = Dom.bot ()
  let otherstate v = Dom.bot ()
  let exitstate  v = Dom.bot ()
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

module Spec2 : Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "spec" (module Spec2 : Spec2)
