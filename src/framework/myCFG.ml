(** Our Control-flow graph implementation. *)

module GU = Goblintutil
module CF = Cilfacade
open Cil
open Pretty
open GobConfig

(* Instead of including all of Node (which pollutes with type t, etc), include explicit aliases (with constructors) *)
type node = Node.t =
  | Statement of CilType.Stmt.t
  (** The statements as identified by CIL *)
  | FunctionEntry of CilType.Fundec.t
  (** *)
  | Function of CilType.Fundec.t
  (** The variable information associated with the function declaration. *)

type edge = Edge.t =
  | Assign of CilType.Lval.t * CilType.Exp.t
  (** Assignments lval = exp *)
  | Proc of CilType.Lval.t option * CilType.Exp.t * CilType.Exp.t list
  (** Function calls of the form lva = fexp (e1, e2, ...) *)
  | Entry of CilType.Fundec.t
  (** Entry edge that relates function declaration to function body. You can use
    * this to initialize the local variables. *)
  | Ret of CilType.Exp.t option * CilType.Fundec.t
  (** Return edge is between the return statement, which may optionally contain
    * a return value, and the function. The result of the call is then
    * transferred to the function node! *)
  | Test of CilType.Exp.t * bool
  (** The true-branch or false-branch of a conditional exp *)
  | ASM of string list * Edge.asm_out * Edge.asm_in
  (** Inline assembly statements, and the annotations for output and input
    * variables. *)
  | VDecl of CilType.Varinfo.t
  (** VDecl edge for the variable in varinfo. Whether such an edge is there for all
    * local variables or only when it is not possible to pull the declaration up, is
    * determined by alwaysGenerateVarDecl in cabs2cil.ml in CIL. One case in which a VDecl
    * is always there is for VLA. If there is a VDecl edge, it is where the declaration originally
    * appeared *)
  | Skip
  (** This is here for historical reasons. I never use Skip edges! *)
  | SelfLoop
  (** This for interrupt edges.! *)

let rec pretty_edges () = function
  | [] -> Pretty.dprintf ""
  | [_,x] -> Edge.pretty () x
  | (_,x)::xs -> Pretty.dprintf "%a; %a" Edge.pretty x pretty_edges xs


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
let dummy_node = FunctionEntry Cil.dummyFunDec

let all_array_index_exp : exp = CastE(TInt(Cilfacade.ptrdiff_ikind (),[]), unknown_exp)

let getLoc (node: node) =
  match node with
  | Statement stmt -> get_stmtLoc stmt.skind
  | Function fv -> fv.svar.vdecl
  | FunctionEntry fv -> fv.svar.vdecl
  

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


let get_containing_function (stmt: stmt): fundec = Hashtbl.find stmt_fundec_map stmt.sid

let getFun (node: node) =
  match node with
  | Statement stmt -> get_containing_function stmt
  | Function fv -> fv
  | FunctionEntry fv -> fv

let printFun (module Cfg : CfgBidir) live fd out =
  (* let out = open_out "cfg.dot" in *)
  let module NH = Hashtbl.Make (Node) in
  let ready      = NH.create 113 in
  let node_table = NH.create 113 in
  let _ = Printf.fprintf out "digraph cfg {\n" in
  let p_node () = function
    | Statement stmt  -> Pretty.dprintf "%d" stmt.sid
    | Function f      -> Pretty.dprintf "ret%d" f.svar.vid
    | FunctionEntry f -> Pretty.dprintf "fun%d" f.svar.vid
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
      | Function f      -> "label =\"return of "^f.svar.vname^"()\",shape=box"
      | FunctionEntry f -> "label =\""^f.svar.vname^"()\",shape=box"
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
  printNode (Function fd);
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
