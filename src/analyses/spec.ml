open Cil
open Pretty
open Analyses
open Batteries

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

  let nodes = ref []
  let edges = ref []

  let load_specfile () =
    let specfile = GobConfig.get_string "spec.file" in
    if String.length specfile < 1 then failwith "You need to specify a specification file using --sets spec.file path/to/file.spec when using the spec analysis!";
    if not (Sys.file_exists specfile) then failwith ("The given spec.file ("^specfile^") doesn't exist (CWD is "^Sys.getcwd ()^").");
    let _nodes, _edges = Ispec.parseFile specfile in
    nodes := _nodes; edges := _edges (* don't change -> no need to save them in domain? *)


  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let m = ctx.local in
    m

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
      | _ -> Dom.V.create stack_var (f []) "" in
    Dom.add stack_var (Must v) m

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let m = ctx.local in
    M.write ("entering function "^f.vname^(callStackStr m));
    if f.vname = "main" then load_specfile ();
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else m in [m,m]

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

(*
.spec-format:
- The file contains two types of definitions: nodes and edges. The labels of nodes are output. The labels of edges are the constraints.
- The start node of the first transition is the start node of the automaton.
- Nodes starting with 'w' are warnings, which have an implicit back edge to the previous node.
- Nodes starting with 'f'/'e' (fail/error/exit/end?) are end nodes.
- An edge with '$_' matches everything and forwards it to the target node.
*)
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
    (* go through constraints and return result on the first match *)
    (* TODO what should be done if multiple constraints would match? *)
    let findSome f xs not_found =
      try let x = List.find (fun x -> match f x with Some _ -> true | _ -> false) xs in
        match f x with Some x -> x | _ -> not_found
      with Not_found -> not_found
    in
    let matching (a,b,c) =
      (* TODO do more in parser *)
      None
      (* Dom.goto var dloc b *)
    in
    findSome matching !edges dummy
(*     match lval, f.vname, arglist with
      | None, "fopen", _ ->
          M.report "file handle is not saved!"; dummy
      | Some lval, "fopen", _ ->
          let f m varinfo =
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
      | _ -> dummy *)

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
