(** Our Control-flow graph implementation. *)

module GU = Goblintutil
module CF = Cilfacade
open Cil
open Deriving.Cil
open Pretty
open GobConfig
include Node

type asm_out = (string option * string * lval) list [@@deriving to_yojson]
type asm_in  = (string option * string * exp ) list [@@deriving to_yojson]

type edge =
  | Assign of lval * exp
  (** Assignments lval = exp *)
  | Proc of lval option * exp * exp list
  (** Function calls of the form lva = fexp (e1, e2, ...) *)
  | Entry of fundec
  (** Entry edge that relates function declaration to function body. You can use
    * this to initialize the local variables. *)
  | Ret of exp option * fundec
  (** Return edge is between the return statement, which may optionally contain
    * a return value, and the function. The result of the call is then
    * transferred to the function node! *)
  | Test of exp * bool
  (** The true-branch or false-branch of a conditional exp *)
  | ASM of string list * asm_out * asm_in
  (** Inline assembly statements, and the annotations for output and input
    * variables. *)
  | VDecl of varinfo
  (** VDecl edge for the variable in varinfo. Whether such an edge is there for all
    * local variables or only when it is not possible to pull the declaration up, is
    * determined by alwaysGenerateVarDecl in cabs2cil.ml in CIL. One case in which a VDecl
    * is always there is for VLA. If there is a VDecl edge, it is where the declaration originally
    * appeared *)
  | Skip
  (** This is here for historical reasons. I never use Skip edges! *)
  | SelfLoop
  (** This for interrupt edges.! *)
[@@deriving to_yojson]


let pretty_edge () = function
  | Assign (lv,rv) -> dprintf "Assign '%a = %a' " d_lval lv d_exp rv
  | Proc (None  ,f,ars) -> dprintf "Proc '%a(%a)'" d_exp f (d_list ", " d_exp) ars
  | Proc (Some r,f,ars) -> dprintf "Proc '%a = %a(%a)'" d_lval r d_exp f (d_list ", " d_exp) ars
  | Entry f -> dprintf "Entry %s" f.svar.vname
  | Ret (None,fd) -> dprintf "Ret (None, %s)" fd.svar.vname
  | Ret (Some r,fd) -> dprintf "Ret (Some %a, %s)" d_exp r fd.svar.vname
  | Test (p,b) -> dprintf "Test (%a,%b)" d_exp p b
  | ASM _ -> text "ASM ..."
  | Skip -> text "Skip"
  | VDecl v -> dprintf "VDecl '%a %s;'" d_type v.vtype v.vname
  | SelfLoop -> text "SelfLoop"

let rec pretty_edges () = function
  | [] -> Pretty.dprintf ""
  | [_,x] -> pretty_edge () x
  | (_,x)::xs -> Pretty.dprintf "%a; %a" pretty_edge x pretty_edges xs

let pretty_edge_kind () = function
  | Assign (lv,rv) -> dprintf "Assign"
  | Proc (_  ,f,ars) -> dprintf "Proc"
  | Entry f -> dprintf "Entry %s" f.svar.vname
  | Ret (r,fd) -> dprintf "Ret"
  | Test (p,b) -> dprintf "Test"
  | ASM _ -> text "ASM"
  | Skip -> text "Skip"
  | VDecl _ -> text "VDecl"
  | SelfLoop -> text "SelfLoop"

type cfg = node -> ((location * edge) list * node) list

module type CfgBackward =
sig
  val prev : node -> ((location * edge) list * node) list
end

module type CfgForward =
sig
  val next : node -> ((location * edge) list * node) list
end

module type CfgBidir =
sig
  include CfgBackward
  include CfgForward
end

module H = BatHashtbl.Make(Node)

(* Dumps a statement to the standard output *)
let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

(* Map from statement id to containing fundec *)
let stmt_fundec_map = Hashtbl.create 113
let current_node : node option ref = ref None

let do_the_params (fd: fundec) =
  (* This function used to create extra variables, but now it just sets the
   * vdecl to -3, lovely... *)
  let create_extra_var (p: varinfo): unit =
    match p.vtype with
    | TPtr (t,_) -> p.vdecl <- {p.vdecl with line = -3 }
    | _ -> p.vdecl <- {p.vdecl with line = -3 }
  in
  List.iter create_extra_var fd.sformals

let unknown_exp : exp = mkString "__unknown_value__"
let dummy_func = emptyFunction "__goblint_dummy_init" (* TODO get rid of this? *)
let dummy_node = FunctionEntry Cil.dummyFunDec.svar

let all_array_index_exp : exp = CastE(TInt(Cilfacade.ptrdiff_ikind (),[]), unknown_exp)

let getLoc (node: node) =
  match node with
  | Statement stmt -> get_stmtLoc stmt.skind
  | Function fv -> fv.vdecl
  | FunctionEntry fv -> fv.vdecl

(* TODO: refactor duplication with find_loop_heads *)
module NH = Hashtbl.Make (Node)
module NS = Set.Make (Node)
let find_loop_heads_fun (module Cfg:CfgForward) (fd:Cil.fundec): unit NH.t =
  let loop_heads = NH.create 100 in
  let global_visited_nodes = NH.create 100 in

  (* DFS *)
  let rec iter_node path_visited_nodes node =
    if NS.mem node path_visited_nodes then
      NH.add loop_heads node ()
    else if not (NH.mem global_visited_nodes node) then begin
      NH.add global_visited_nodes node ();
      let new_path_visited_nodes = NS.add node path_visited_nodes in
      List.iter (fun (_, to_node) ->
          iter_node new_path_visited_nodes to_node
        ) (Cfg.next node)
    end
  in

  let entry_node = FunctionEntry fd.svar in
  iter_node NS.empty entry_node;

  loop_heads

let find_backwards_reachable (module Cfg:CfgBackward) (node:node): unit NH.t =
  let reachable = NH.create 100 in

  (* DFS, copied from Control is_sink *)
  let rec iter_node node =
    if not (NH.mem reachable node) then begin
      NH.replace reachable node ();
      List.iter (fun (_, prev_node) ->
          iter_node prev_node
        ) (Cfg.prev node)
    end
  in

  iter_node node;
  reachable

let createCFG (file: file) =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  if Messages.tracing then Messages.trace "cfg" "Starting to build the cfg.\n\n";

  (* Utility function to add stmt edges to the cfg *)
  let addCfg' t xs f =
    if Messages.tracing then
      Messages.trace "cfg" "Adding edges [%a] from\n\t%a\nto\n\t%a ... "
        pretty_edges xs
        pretty_short_node f
        pretty_short_node t;
    H.add cfgB t (xs,f);
    H.add cfgF f (xs,t);
    Messages.trace "cfg" "done\n\n"
  in
  let addCfg t (e,f) = addCfg' t [getLoc f,e] f in
  let mkEdge fromNode edge toNode = addCfg (Statement toNode) (edge, Statement fromNode) in
  let mkEdges fromNode edges toNode = addCfg' toNode edges fromNode in
  (* Function for finding the next real successor of a statement. CIL tends to
   * put a lot of junk between stuff: *)
  let realnode is_entry stmt =
    if Messages.tracing then Messages.tracei "cfg" "realnode is_entry=%B stmt=%d\n" is_entry stmt.sid;
    let rec realnode is_entry visited stmt =
      if Messages.tracing then Messages.trace "cfg" "realnode inner is_entry=%B visited=[%a] stmt=%d: %a\n" is_entry (d_list "; " (fun () x -> Pretty.text (string_of_int x))) visited stmt.sid dn_stmt stmt;
      if List.mem stmt.sid visited then stmt
      else match (List.hd stmt.succs) with
        | exception (Failure _) -> if is_entry then stmt else raise Not_found
        | next -> begin
            let sid = stmt.sid in
            match stmt.skind with
            | Block _ -> realnode is_entry (sid::visited) next
            | Goto _ -> realnode is_entry (sid::visited) next
            | Instr [] -> begin
                if next.sid == stmt.sid
                then stmt
                else realnode is_entry (sid::visited) next
              end
            | Loop _ -> realnode is_entry (sid::visited) next
            | If (exp,_,_,_) -> if isZero exp then realnode is_entry (sid::visited) next else stmt
            | _ -> stmt
          end
    in
    try
      let r = realnode is_entry [] stmt in
      if Messages.tracing then Messages.traceu "cfg" "-> %d\n" r.sid;
      r
    with Not_found ->
      if Messages.tracing then Messages.traceu "cfg" "-> Not_found\n";
      raise Not_found
  in
  addCfg (Function dummy_func.svar) (Ret (None, dummy_func), FunctionEntry dummy_func.svar);
  (* We iterate over all globals looking for functions: *)
  iterGlobals file (fun glob ->
      match glob with
      | GFun (fd,loc) ->
        if Messages.tracing then Messages.trace "cfg" "Looking at the function %s.\n" fd.svar.vname;

        Cfg.printCfgFilename ("cilcfg." ^ fd.svar.vname ^ ".dot") fd;
        (* Walk through the parameters and pre-process them a bit... *)
        do_the_params fd;
        (* Find the first statement in the function *)
        let entrynode = realnode true (CF.getFirstStmt fd) in
        (* Add the entry edge to that node *)
        let _ = addCfg (Statement entrynode) ((Entry fd), (FunctionEntry fd.svar)) in
        (* Return node to be used for infinite loop connection to end of function
         * lazy, so it's only added when actually needed *)
        let pseudo_return = lazy (
          let newst = mkStmt (Return (None, loc)) in
          let start_id = 10_000_000_000 in (* TODO get max_sid? *)
          let sid = Hashtbl.hash loc in (* Need pure sid instead of Cil.new_sid for incremental, similar to vid in Goblintutil.create_var. We only add one return stmt per loop, so the location hash should be unique. *)
          newst.sid <- if sid < start_id then sid + start_id else sid;
          Hashtbl.add stmt_fundec_map newst.sid fd;
          let newst_node = Statement newst in
          addCfg (Function fd.svar) (Ret (None,fd), newst_node);
          newst_node
        )
        in
        (* So for each statement in the function body, we do the following: *)
        let handle stmt =
          (* Please ignore the next line. It creates an index of statements
           * so the Eclipse plug-in can know what function a given result
           * belongs to. *)
          Hashtbl.add stmt_fundec_map stmt.sid fd;
          if Messages.tracing then Messages.trace "cfg" "Statement %d at %a.\n" stmt.sid d_loc (get_stmtLoc stmt.skind);
          match stmt.skind with
          (* turn pthread_exit into return? *)
          (* | Instr [Call (_, Lval (Var {vname="pthread_exit"}, _), [ret_exp], _)] -> addCfg (Function fd.svar) (Ret (Some ret_exp,fd), Statement stmt) *)
          (* | Instr [Call (lval, ((Lval (Var {vname="pthread_exit"}, _)) as func), ([ret_exp] as args), loc)] -> addCfg (Function fd.svar) (Proc (lval, func, args), Statement stmt) *)
          (* Normal instructions are easy. They should be a list of a single
           * instruction, either Set, Call or ASM: *)
          | Instr xs ->
            (* We need to add an edge to each of the successors of the current statement *)
            let handle_instr = function
              | Set (lval,exp,loc) -> loc, Assign (lval, exp)
              | Call (lval,func,args,loc) -> loc, Proc (lval,func,args)
              | Asm (attr,tmpl,out,inp,regs,loc) -> loc, ASM (tmpl,out,inp)
              | VarDecl (v, loc) -> loc, VDecl(v)
            in
            let handle_instrs' = function
              (* Empty Instrs are weird: they have edges without any label or transfer function.
               * Instead turn them into Skips, which keep them in Goblint's CFG for witness use. *)
              | [] -> [Cil.locUnknown, Skip] (* TODO: better loc from somewhere? *)
              | xs -> List.map handle_instr xs
            in
            let handle_instrs succ = mkEdges (Statement stmt) (handle_instrs' xs) succ in
            (* Sometimes a statement might not have a successor.
             * This can happen if the last statement of a function is a call to exit. *)
            let succs = if stmt.succs = [] then [Lazy.force pseudo_return] else List.map (fun x -> Statement (realnode true x)) stmt.succs in
            List.iter handle_instrs succs
          (* If expressions are a bit more interesting, but CIL has done
           * its job well and we just pick out the right successors *)
          | If (exp, true_block, false_block, loc) -> begin
              if isZero exp then ()
              else
                let false_stmt = realnode true (List.nth stmt.succs 0) in
                let true_stmt = try
                    realnode true (List.nth stmt.succs 1)
                  with Failure _ -> realnode true (List.hd stmt.succs) in
                mkEdge stmt (Test (exp, true )) true_stmt;
                mkEdge stmt (Test (exp, false)) false_stmt
            end
          (* Loops can generally be ignored because CIL creates gotos for us,
           * except constant conditions are eliminated, so non-terminating
           * loops are not connected to the rest of the code. This is a
           * problem for side-effecting demand driven solvers. I add one
           * extra edge that is always false to the exit of the loop. *)
          | Loop (bl,loc,Some cont, Some brk) -> begin
            if Messages.tracing then Messages.trace "cfg" "loop %d cont=%d brk=%d\n" stmt.sid cont.sid brk.sid;
              (* try
                    mkEdge (realnode true stmt) (Test (one, false)) (realnode false brk);
                  with
                  (* The [realnode brk] fails when the break label is at the end
                  * of the function. In that case, we need to connect it to
                  * the [Call] node. *)
                  | Not_found ->
                    addCfg (Lazy.force pseudo_return) (Test (one, false), Statement (realnode true stmt)) (* TODO: is this necessary anymore? maybe could just leave it out and let the general case below handle it *) *)

            end
          (* The return edges are connected to the function *)
          | Return (exp,loc) -> addCfg (Function fd.svar) (Ret (exp,fd), Statement stmt)
          (* Gotos are skipped over by realnode and usually not needed.
           * Except goto loops with empty bodies need the Skip edge to be identified as loop heads and connected to return.
           * This also creates some unconnected but unnecessary edges which are covered by realnode. *)
          | Goto (target_ref, loc) ->
            if !target_ref.sid = stmt.sid then
              addCfg (Statement !target_ref) (Skip, Statement stmt)
          | _ ->
            if Messages.tracing then Messages.trace "cfg" "Unknown stmtkind for %a\n" d_stmt stmt
        in
        List.iter handle fd.sallstmts;

        if Messages.tracing then Messages.trace "cfg" "Over\n";

        (* Connect remaining infinite loops (e.g made using goto) to end of function
         * via pseudo return node for demand driven solvers *)
        let module TmpCfg: CfgBidir =
        struct
          let next = H.find_all cfgF
          let prev = H.find_all cfgB
        end
        in
        let loop_heads = find_loop_heads_fun (module TmpCfg) fd in
        let reachable_return = find_backwards_reachable (module TmpCfg) (Function fd.svar) in
        NH.iter (fun node () ->
            if not (NH.mem reachable_return node) then
              addCfg (Lazy.force pseudo_return) (Test (one, false), node)
          ) loop_heads
      | _ -> ()
    );
  if Messages.tracing then Messages.trace "cfg" "CFG building finished.\n\n";
  cfgF, cfgB

let print cfg  =
  let out = open_out "cfg.dot" in
  let module NH = Hashtbl.Make (Node) in
  let node_table = NH.create 113 in
  let _ = Printf.fprintf out "digraph cfg {\n" in
  let p_node () = function
    | Statement stmt  -> Pretty.dprintf "%d" stmt.sid
    | Function f      -> Pretty.dprintf "ret%d" f.vid
    | FunctionEntry f -> Pretty.dprintf "fun%d" f.vid
  in
  let dn_exp () e =
    text (XmlUtil.escape (sprint 800 (dn_exp () e)))
  in
  let dn_lval () l =
    text (XmlUtil.escape (sprint 800 (dn_lval () l)))
  in
  let p_edge () = function
    | Test (exp, b) -> if b then Pretty.dprintf "Pos(%a)" dn_exp exp else Pretty.dprintf "Neg(%a)" dn_exp exp
    | Assign (lv,rv) -> Pretty.dprintf "%a = %a" dn_lval lv dn_exp rv
    | Proc (Some ret,f,args) -> Pretty.dprintf "%a = %a(%a)" dn_lval ret dn_exp f (d_list ", " dn_exp) args
    | Proc (None,f,args) -> Pretty.dprintf "%a(%a)" dn_exp f (d_list ", " dn_exp) args
    | Entry (f) -> Pretty.text "(body)"
    | Ret (Some e,f) -> Pretty.dprintf "return %a" dn_exp e
    | Ret (None,f) -> Pretty.dprintf "return"
    | ASM (_,_,_) -> Pretty.text "ASM ..."
    | Skip -> Pretty.text "skip"
    | VDecl v -> Cil.defaultCilPrinter#pVDecl () v
    | SelfLoop -> Pretty.text "SelfLoop"
  in
  (* escape string in label, otherwise dot might fail *)
  (* Weirdly, this actually causes xdot to fail with \v in string literals. *)
  (* let p_edge_escaped () x = Pretty.text (String.escaped (Pretty.sprint ~width:0 (Pretty.dprintf "%a" p_edge x))) in *)
  let rec p_edges () = function
    | [] -> Pretty.dprintf ""
    | (_,x)::xs -> Pretty.dprintf "%a\n%a" p_edge x p_edges xs
  in
  let printNodeStyle (n:node) () =
    match n with
    | Statement {skind=If (_,_,_,_); _} as s  -> ignore (Pretty.fprintf out "\t%a [shape=diamond]\n" p_node s)
    | Statement stmt  -> ()
    | Function f      -> ignore (Pretty.fprintf out "\t%a [label =\"return of %s()\",shape=box];\n" p_node (Function f) f.vname)
    | FunctionEntry f -> ignore (Pretty.fprintf out "\t%a [label =\"%s()\",shape=box];\n" p_node (FunctionEntry f) f.vname)
  in
  let printEdge (toNode: node) ((edges:(location * edge) list), (fromNode: node)) =
    ignore (Pretty.fprintf out "\t%a -> %a [label = \"%a\"] ;\n" p_node fromNode p_node toNode p_edges edges);
    NH.add node_table toNode ();
    NH.add node_table fromNode ()
  in
  H.iter printEdge cfg;
  NH.iter printNodeStyle node_table;
  Printf.fprintf out "}\n";
  flush out;
  close_out_noerr out

let getGlobalInits (file: file) : (edge * location) list  =
  (* runtime with fast_global_inits: List: 36.25s, Hashtbl: 0.56s *)
  let inits = Hashtbl.create 13 in
  let fast_global_inits = get_bool "exp.fast_global_inits" in
  let rec doInit lval loc init is_zero =
    let initoffs offs init typ lval =
      doInit (addOffsetLval offs lval) loc init is_zero;
      lval
    in
    let rec all_index = function
      | Index (e,o) -> Index (all_array_index_exp, all_index o)
      | Field (f,o) -> Field (f, all_index o)
      | NoOffset -> NoOffset
    in
    let all_index (lh,offs) = lh, all_index offs in
    match init with
    | SingleInit exp ->
      let assign lval = Assign (lval, exp), loc in
      (* This is an optimization so that we don't get n*m assigns for an array a[n][m].
         Instead, we get one assign for each distinct value in the array *)
      if not fast_global_inits then
        Hashtbl.add inits (assign lval) ()
      else if not (Hashtbl.mem inits (assign (all_index lval))) then
        Hashtbl.add inits (assign (all_index lval)) ()
      else
        ()
    | CompoundInit (typ, lst) ->
      let ntyp = match typ, lst with
        | TArray(t, None, attr), [] -> TArray(t, Some zero, attr) (* set initializer type to t[0] for flexible array members of structs that are intialized with {} *)
        | _, _ -> typ
      in
      ignore (foldLeftCompound ~implicit:true ~doinit:initoffs ~ct:ntyp ~initl:lst ~acc:lval)
  in
  let f glob =
    match glob with
    | GVar ({vtype=vtype; _} as v, init, loc) -> begin
        let init, is_zero = match init.init with
          | None -> makeZeroInit vtype, true
          | Some x -> x, false
        in
        doInit (var v) loc init is_zero
      end
    | _ -> ()
  in
  iterGlobals file f;
  let initfun = emptyFunction "__goblint_dummy_init" in
  (* order is not important since only compile-time constants can be assigned *)
  (Entry initfun, {line = 0; file="initfun"; byte= 0} ) :: (BatHashtbl.keys inits |> BatList.of_enum)

let numGlobals file =
  let n = ref 0 in
  (* GVar Cannot have storage Extern or function type *)
  Cil.iterGlobals file (function GVar _ -> incr n | _ -> ());
  !n

let generate_irpt_edges cfg =
  let make_irpt_edge toNode (_, fromNode) =
    match toNode with
    | FunctionEntry f -> let _ = print_endline ( " Entry " ) in ()
    | _ -> H.add cfg toNode (SelfLoop, toNode)
  in
  H.iter make_irpt_edge cfg

let minimizeCFG (fw,bw) =
  let keep = H.create 113 in
  let comp_keep t (_,f) =
    if (List.length (H.find_all bw t)<>1) || (List.length (H.find_all fw t)<>1) then
      H.replace keep t ();
    if (List.length (H.find_all bw f)<>1) || (List.length (H.find_all fw f)<>1) then
      H.replace keep f ()
  in
  H.iter comp_keep bw;
  (* H.iter comp_keep fw; *)
  let cfgB = H.create 113 in
  let cfgF = H.create 113 in
  let ready = H.create 113 in
  let rec add a b t (e,f)=
    if H.mem keep f then begin
      H.add cfgB b (e@a,f);
      H.add cfgF f (e@a,b);
      if H.mem ready b then begin
        H.replace ready f ();
        List.iter (add [] f f) (H.find_all bw f)
      end
    end else begin
      List.iter (add (e@a) b f) (H.find_all bw f)
    end
  in
  H.iter (fun k _ -> List.iter (add [] k k) (H.find_all bw k)) keep;
  H.clear ready;
  H.clear keep;
  cfgF, cfgB

let getCFG (file: file) : cfg * cfg =
  let cfgF, cfgB = createCFG file in
  let cfgF, cfgB =
    if get_bool "exp.mincfg" then
      Stats.time "minimizing the cfg" minimizeCFG (cfgF, cfgB)
    else
      (cfgF, cfgB)
  in
  if get_bool "justcfg" then print cfgB;
  H.find_all cfgF, H.find_all cfgB

let get_containing_function (stmt: stmt): fundec = Hashtbl.find stmt_fundec_map stmt.sid

let getFun (node: node) =
  match node with
  | Statement stmt -> get_containing_function stmt
  | Function fv -> CF.getdec fv
  | FunctionEntry fv -> CF.getdec fv

let printFun (module Cfg : CfgBidir) live fd out =
  (* let out = open_out "cfg.dot" in *)
  let module NH = Hashtbl.Make (Node) in
  let ready      = NH.create 113 in
  let node_table = NH.create 113 in
  let _ = Printf.fprintf out "digraph cfg {\n" in
  let p_node () = function
    | Statement stmt  -> Pretty.dprintf "%d" stmt.sid
    | Function f      -> Pretty.dprintf "ret%d" f.vid
    | FunctionEntry f -> Pretty.dprintf "fun%d" f.vid
  in
  let dn_exp () e =
    text (XmlUtil.escape (sprint 800 (dn_exp () e)))
  in
  let dn_lval () l =
    text (XmlUtil.escape (sprint 800 (dn_lval () l)))
  in
  let p_edge () = function
    | Test (exp, b) -> if b then Pretty.dprintf "Pos(%a)" dn_exp exp else Pretty.dprintf "Neg(%a)" dn_exp exp
    | Assign (lv,rv) -> Pretty.dprintf "%a = %a" dn_lval lv dn_exp rv
    | Proc (Some ret,f,args) -> Pretty.dprintf "%a = %a(%a)" dn_lval ret dn_exp f (d_list ", " dn_exp) args
    | Proc (None,f,args) -> Pretty.dprintf "%a(%a)" dn_exp f (d_list ", " dn_exp) args
    | Entry (f) -> Pretty.text "(body)"
    | Ret (Some e,f) -> Pretty.dprintf "return %a" dn_exp e
    | Ret (None,f) -> Pretty.dprintf "return"
    | ASM (_,_,_) -> Pretty.text "ASM ..."
    | Skip -> Pretty.text "skip"
    | VDecl v -> Cil.defaultCilPrinter#pVDecl () v
    | SelfLoop -> Pretty.text "SelfLoop"
  in
  let rec p_edges () = function
    | [] -> Pretty.dprintf ""
    | (_,x)::xs -> Pretty.dprintf "%a\n%a" p_edge x p_edges xs
  in
  let printNodeStyle (n:node) () =
    let liveness = if live n then "fillcolor=white,style=filled" else "fillcolor=orange,style=filled" in
    let kind_style =
      match n with
      | Statement {skind=If (_,_,_,_); _}  -> "shape=diamond"
      | Statement stmt  -> ""
      | Function f      -> "label =\"return of "^f.vname^"()\",shape=box"
      | FunctionEntry f -> "label =\""^f.vname^"()\",shape=box"
    in
    ignore (Pretty.fprintf out ("\t%a [id=\"%a\",URL=\"javascript:show_info('\\N');\",%s,%s];\n") p_node n p_node n liveness kind_style)
  in
  let printEdge (toNode: node) ((edges:(location * edge) list), (fromNode: node)) =
    ignore (Pretty.fprintf out "\t%a -> %a [label = \"%a\"] ;\n" p_node fromNode p_node toNode p_edges edges);
    NH.replace node_table toNode ();
    NH.replace node_table fromNode ()
  in
  let rec printNode (toNode : node) =
    if not (NH.mem ready toNode) then begin
      NH.add ready toNode ();
      let prevs = Cfg.prev toNode in
      List.iter (printEdge toNode) prevs;
      List.iter (fun (_,x) -> printNode x) prevs
    end
  in
  printNode (Function fd.svar);
  NH.iter printNodeStyle node_table;
  Printf.fprintf out "}\n";
  flush out;
  close_out_noerr out

let dead_code_cfg (file:file) (module Cfg : CfgBidir) live =
  iterGlobals file (fun glob ->
      match glob with
      | GFun (fd,loc) ->
        (* ignore (Printf.printf "fun: %s\n" fd.svar.vname); *)
        let base_dir = Goblintutil.create_dir ((if get_bool "interact.enabled" then get_string "interact.out"^"/" else "")^"cfgs") in
        let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") fd.svar.vdecl.file in
        let dot_file_name = fd.svar.vname^".dot" in
        let file_dir = Goblintutil.create_dir (Filename.concat base_dir c_file_name) in
        let fname = Filename.concat file_dir dot_file_name in
        printFun (module Cfg : CfgBidir) live fd (open_out fname)
      | _ -> ()
    )
