(** Signatures for analyzers, analysis specifications, and result output.  *)

open Cil
open Pretty
open GobConfig

module GU = Goblintutil
module M  = Messages

(** Analysis starts from lists of functions: start functions, exit functions, and
  * other functions. *)
type fundecs = fundec list * fundec list * fundec list

module type VarType =
sig
  include Hashtbl.HashedType
  val pretty_trace: unit -> t -> doc
  val compare : t -> t -> int
  val category : t -> int

  val printXml : 'a BatInnerIO.output -> t -> unit
  val var_id   : t -> string
  val file_name : t -> string
  val line_nr   : t -> int
  val node      : t -> MyCFG.node
  val relift    : t -> t (* needed only for incremental+hashcons to re-hashcons contexts after loading *)
end

module Var =
struct
  type t = MyCFG.node [@@deriving eq, ord]
  let relift x = x

  let category = function
    | MyCFG.Statement     s -> 1
    | MyCFG.Function      f -> 2
    | MyCFG.FunctionEntry f -> 3

  let hash x =
    match x with
    | MyCFG.Statement     s -> Hashtbl.hash (s.sid, 0)
    | MyCFG.Function      f -> Hashtbl.hash (f.svar.vid, 1)
    | MyCFG.FunctionEntry f -> Hashtbl.hash (f.svar.vid, 2)

  let getLocation n = MyCFG.getLoc n

  let pretty () x =
    match x with
    | MyCFG.Statement     s -> dprintf "node %d \"%a\"" s.sid Basetype.CilStmt.pretty s
    | MyCFG.Function      f -> dprintf "call of %s" f.svar.vname
    | MyCFG.FunctionEntry f -> dprintf "entry state of %s" f.svar.vname

  let pretty_trace () x =  dprintf "%a on %a" pretty x CilType.Location.pretty (getLocation x)

  let kind = function
    | MyCFG.Function f                         -> `ExitOfProc f
    | MyCFG.Statement {skind = Instr [Call _]; _} -> `ProcCall
    | _ -> `Other

  let printXml f n =
    let id ch n =
      match n with
      | MyCFG.Statement s     -> BatPrintf.fprintf ch "%d" s.sid
      | MyCFG.Function f      -> BatPrintf.fprintf ch "ret%d" f.svar.vid
      | MyCFG.FunctionEntry f -> BatPrintf.fprintf ch "fun%d" f.svar.vid
    in
    let l = MyCFG.getLoc n in
    BatPrintf.fprintf f "<call id=\"%a\" file=\"%s\" fun=\"%s\" line=\"%d\" order=\"%d\">\n" id n l.file (MyCFG.getFun n).svar.vname l.line l.byte

  let var_id n =
    match n with
    | MyCFG.Statement s     -> string_of_int s.sid
    | MyCFG.Function f      -> "ret" ^ string_of_int f.svar.vid
    | MyCFG.FunctionEntry f -> "fun" ^ string_of_int f.svar.vid

  let line_nr n = (MyCFG.getLoc n).line
  let file_name n = (MyCFG.getLoc n).file
  let description n = sprint 80 (pretty () n)
  let context () _ = Pretty.nil
  let node n = n
end


module VarF (LD: Printable.S) =
struct
  type t = MyCFG.node * LD.t [@@deriving eq, ord]
  let relift (n,x) = n, LD.relift x

  let category = function
    | (MyCFG.Statement     s,_) -> 1
    | (MyCFG.Function      f,_) -> 2
    | (MyCFG.FunctionEntry f,_) -> 3

  let hashmul x y = if x=0 then y else if y=0 then x else x*y
  let hash x =
    match x with
    | (MyCFG.Statement     s,d) -> hashmul (LD.hash d) (s.sid*17)
    | (MyCFG.Function      f,d) -> hashmul (LD.hash d) (f.svar.vid*19)
    | (MyCFG.FunctionEntry f,d) -> hashmul (LD.hash d) (f.svar.vid*23)

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () x =
    match x with
    | (MyCFG.Statement     s,d) -> dprintf "node %d \"%a\"" s.sid Basetype.CilStmt.pretty s
    | (MyCFG.Function      f,d) -> dprintf "call of %s" f.svar.vname
    | (MyCFG.FunctionEntry f,d) -> dprintf "entry state of %s" f.svar.vname

  let pretty_trace () (n,c as x) =
    if get_bool "dbg.trace.context" then dprintf "(%a, %a) on %a \n" pretty x LD.pretty c CilType.Location.pretty (getLocation x)
    else dprintf "%a on %a" pretty x CilType.Location.pretty (getLocation x)

  let printXml f (n,c) =
    Var.printXml f n;
    BatPrintf.fprintf f "<context>\n";
    LD.printXml f c;
    BatPrintf.fprintf f "</context>\n"

  let var_id (n,_) = Var.var_id n

  let line_nr (n,_) = (MyCFG.getLoc n).line
  let file_name (n,_) = (MyCFG.getLoc n).file
  let description (n,_) = sprint 80 (Var.pretty () n)
  let context () (_,c) = LD.pretty () c
  let node (n,_) = n
end
exception Deadcode


(** [Dom (D)] produces D lifted where bottom means dead-code *)
module Dom (LD: Lattice.S) =
struct
  include Lattice.Lift (LD) (struct
      let bot_name = "Dead code"
      let top_name = "Totally unknown and messed up"
    end)

  let lift (x:LD.t) : t = `Lifted x

  let unlift x =
    match x with
    | `Lifted x -> x
    | _ -> raise Deadcode

  let lifted f x =
    match x with
    | `Lifted x -> `Lifted (f x)
    | tb -> tb

  let printXml f = function
    | `Top -> BatPrintf.fprintf f "<value>%s</value>" (XmlUtil.escape top_name)
    | `Bot -> ()
    | `Lifted x -> LD.printXml f x
end



module type ResultConf =
sig
  val result_name: string
end

module Result (Range: Printable.S) (C: ResultConf) =
struct
  include Hash.Printable (Basetype.ProgLinesFun) (Range)
  include C

  let printXml f xs =
    let print_id f = function
      | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
      | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.svar.vid
      | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.svar.vid
    in
    let print_one n v =
      let loc = Tracing.getLoc n in
      BatPrintf.fprintf f "<call id=\"%a\" file=\"%s\" line=\"%d\" order=\"%d\">\n" print_id n loc.file loc.line loc.byte;
      BatPrintf.fprintf f "%a</call>\n" Range.printXml v
    in
    iter print_one xs

  let printJson f xs =
    let print_id f = function
      | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
      | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.svar.vid
      | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.svar.vid
    in
    let print_one n v =
      let loc = Tracing.getLoc n in
      BatPrintf.fprintf f "{\n\"id\": \"%a\", \"file\": \"%s\", \"line\": \"%d\", \"byte\": \"%d\", \"states\": %s\n},\n" print_id n loc.file loc.line loc.byte (Yojson.Safe.to_string (Range.to_yojson v))
    in
    iter print_one xs

  let printXmlWarning f () =
    let one_text f (m,l) =
      BatPrintf.fprintf f "\n<text file=\"%s\" line=\"%d\">%s</text>" l.file l.line (GU.escape m)
    in
    let one_w f = function
      | `text (m,l)  -> one_text f (m,l)
      | `group (n,e) ->
        BatPrintf.fprintf f "<group name=\"%s\">%a</group>\n" n (BatList.print ~first:"" ~last:"" ~sep:"" one_text) e
    in
    let one_w f x = BatPrintf.fprintf f "\n<warning>%a</warning>" one_w x in
    List.iter (one_w f) !Messages.warning_table

  let printXmlGlobals f () =
    let one_text f (m,l) =
      BatPrintf.fprintf f "\n<text file=\"%s\" line=\"%d\">%s</text>" l.file l.line m
    in
    let one_w f = function
      | `text (m,l)  -> one_text f (m,l)
      | `group (n,e) ->
        BatPrintf.fprintf f "<group name=\"%s\">%a</group>\n" n (BatList.print ~first:"" ~last:"" ~sep:"" one_text) e
    in
    List.iter (one_w f) !Messages.warning_table

  let output table gtable gtfxml (file: file) =
    let out = Messages.get_out result_name !GU.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
    | "indented" -> failwith " `indented` is no longer supported for `result`, use fast_xml instead "
    | "compact" -> failwith " `compact` is no longer supported for `result`, use fast_xml instead "
    | "html" -> failwith " `html` is no longer supported for `result`, run with --html instead "
    | "fast_xml" ->
      let module SH = BatHashtbl.Make (Basetype.RawStrings) in
      let file2funs = SH.create 100 in
      let funs2node = SH.create 100 in
      iter (fun n _ -> SH.add funs2node (MyCFG.getFun n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_node f = function
        | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
        | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.svar.vid
        | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.svar.vid
      in
      let p_nodes f xs =
        List.iter (BatPrintf.fprintf f "<node name=\"%a\"/>\n" p_node) xs
      in
      let p_funs f xs =
        let one_fun n =
          BatPrintf.fprintf f "<function name=\"%s\">\n%a</function>\n" n p_nodes (SH.find_all funs2node n)
        in
        List.iter one_fun xs
      in
      let write_file f fn =
        Messages.xml_file_name := fn;
        BatPrintf.printf "Writing xml to temp. file: %s\n%!" fn;
        BatPrintf.fprintf f "<run>";
        BatPrintf.fprintf f "<parameters>%a</parameters>" (BatArray.print ~first:"" ~last:"" ~sep:" " BatString.print) BatSys.argv;
        BatPrintf.fprintf f "<statistics>";
        (* FIXME: This is a super ridiculous hack we needed because BatIO has no way to get the raw channel CIL expects here. *)
        let name, chn = Filename.open_temp_file "stat" "goblint" in
        Stats.print chn "";
        close_out chn;
        let f_in = BatFile.open_in name in
        let s = BatIO.read_all f_in in
        BatIO.close_in f_in;
        BatPrintf.fprintf f "%s" s;
        BatPrintf.fprintf f "</statistics>";
        BatPrintf.fprintf f "<result>\n";
        BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (BatEnum.uniq @@ SH.keys file2funs);
        BatPrintf.fprintf f "%a" printXml (Lazy.force table);
        gtfxml f gtable;
        printXmlWarning f ();
        BatPrintf.fprintf f "</result></run>\n";
        BatPrintf.fprintf f "%!"
      in
      if get_bool "g2html" then
        BatFile.with_temporary_out ~mode:[`create;`text;`delete_on_exit] write_file
      else
        let f = BatIO.output_channel out in
        write_file f (get_string "outfile")
    | "json" ->
      let open BatPrintf in
      let module SH = BatHashtbl.Make (Basetype.RawStrings) in
      let file2funs = SH.create 100 in
      let funs2node = SH.create 100 in
      iter (fun n _ -> SH.add funs2node (MyCFG.getFun n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_enum p f xs = BatEnum.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      let p_list p f xs = BatList.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      (*let p_kv f (k,p,v) = fprintf f "\"%s\": %a" k p v in*)
      (*let p_obj f xs = BatList.print ~first:"{\n  " ~last:"\n}" ~sep:",\n  " p_kv xs in*)
      let p_node f = function
        | MyCFG.Statement stmt  -> fprintf f "\"%d\"" stmt.sid
        | MyCFG.Function g      -> fprintf f "\"ret%d\"" g.svar.vid
        | MyCFG.FunctionEntry g -> fprintf f "\"fun%d\"" g.svar.vid
      in
      let p_fun f x = fprintf f "{\n  \"name\": \"%s\",\n  \"nodes\": %a\n}" x (p_list p_node) (SH.find_all funs2node x) in
      (*let p_fun f x = p_obj f [ "name", BatString.print, x; "nodes", p_list p_node, SH.find_all funs2node x ] in*)
      let p_file f x = fprintf f "{\n  \"name\": \"%s\",\n  \"path\": \"%s\",\n  \"functions\": %a\n}" (Filename.basename x) x (p_list p_fun) (SH.find_all file2funs x) in
      let write_file f fn =
        printf "Writing json to temp. file: %s\n%!" fn;
        fprintf f "{\n  \"parameters\": \"%a\",\n  " (BatArray.print ~first:"" ~last:"" ~sep:" " BatString.print) BatSys.argv;
        fprintf f "\"files\": %a,\n  " (p_enum p_file) (SH.keys file2funs);
        fprintf f "\"results\": [\n  %a\n]\n" printJson (Lazy.force table);
        (*gtfxml f gtable;*)
        (*printXmlWarning f ();*)
        fprintf f "}\n";
      in
      if get_bool "g2html" then
        BatFile.with_temporary_out ~mode:[`create;`text;`delete_on_exit] write_file
      else
        let f = BatIO.output_channel out in
        write_file f (get_string "outfile")
    (* | "mongo" ->
       let open Deriving.Cil in
       Printf.printf "Connecting to local MongoDB... ";
       let db = Db.connect () in
       let insert (loc,n,fd) v =
         Db.insert db (MyCFG.node_to_yojson n, location_to_yojson loc, Range.to_yojson v)
       in
       let t = Unix.gettimeofday () in
       Printf.printf "Inserting %d entries... " (length (Lazy.force table));
       iter insert (Lazy.force table);
       let t1 = Unix.gettimeofday () -. t in
       Printf.printf "Done in %fs!\n" t1 *)
    | "none" -> ()
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end

(* TODO: unused *)
module ComposeResults (R1: Printable.S) (R2: Printable.S) (C: ResultConf) =
struct
  module R = Printable.Either (R1) (R2)
  module H1 = Hash.Printable (Basetype.ProgLinesFun) (R1)
  module H2 = Hash.Printable (Basetype.ProgLinesFun) (R2)

  include Result (R) (C)

  let merge h1 h2 =
    let hash = create 113 in
    let f k v = add hash k (`Left v) in
    let g k v = add hash k (`Right v) in
    H1.iter f h1;
    H2.iter g h2;
    hash
end


(* Experiment to reduce the number of arguments on transfer functions and allow
   sub-analyses. The list sub contains the current local states of analyses in
   the same order as written in the dependencies list (in MCP).

   The foreign states when calling special_fn or enter are joined if the foreign
   analysis tries to be path-sensitive in these functions. First try to only
   depend on simple analyses.

   It is not clear if we need pre-states, post-states or both on foreign analyses.
*)
type ('d,'g,'c) ctx =
  { ask      : 'a. 'a Queries.t -> 'a Queries.result (* Inlined Queries.ask *)
  ; emit     : Events.t -> unit
  ; node     : MyCFG.node
  ; prev_node: MyCFG.node
  ; control_context : Obj.t (** (Control.get_spec ()) context, represented type: unit -> (Control.get_spec ()).C.t *)
  ; context  : unit -> 'c (** current Spec context *)
  ; edge     : MyCFG.edge
  ; local    : 'd
  ; global   : varinfo -> 'g
  ; presub   : (string * Obj.t) list
  ; postsub  : (string * Obj.t) list
  ; spawn    : lval option -> varinfo -> exp list -> unit
  ; split    : 'd -> Events.t list -> unit
  ; sideg    : varinfo -> 'g -> unit
  ; assign   : ?name:string -> lval -> exp -> unit
  }

exception Ctx_failure of string
(** Failure from ctx, e.g. global initializer *)

let ctx_failwith s = raise (Ctx_failure s) (* TODO: use everywhere in ctx *)

(** Convert [ctx] to [Queries.ask]. *)
let ask_of_ctx ctx: Queries.ask = { Queries.f = fun (type a) (q: a Queries.t) -> ctx.ask q }

let swap_st ctx st =
  {ctx with local=st}

let set_st_gl ctx st gl spawn_tr eff_tr split_tr =
  {ctx with local=st; global=gl; spawn=spawn_tr ctx.spawn; sideg=eff_tr ctx.sideg;
            split=split_tr ctx.split}


module type Spec =
sig
  module D : Lattice.S
  module G : Lattice.S
  module C : Printable.S

  val name : unit -> string

  val init : unit -> unit
  val finalize : unit -> unit
  (* val finalize : G.t -> unit *)

  val startstate : varinfo -> D.t
  val morphstate : varinfo -> D.t -> D.t
  val exitstate  : varinfo -> D.t

  val should_join : D.t -> D.t -> bool
  val val_of  : C.t -> D.t
  val context : D.t -> C.t
  val call_descr : fundec -> C.t -> string

  val sync  : (D.t, G.t, C.t) ctx -> [`Normal | `Join | `Return] -> D.t
  val query : (D.t, G.t, C.t) ctx -> 'a Queries.t -> 'a Queries.result
  val assign: (D.t, G.t, C.t) ctx -> lval -> exp -> D.t
  val vdecl : (D.t, G.t, C.t) ctx -> varinfo -> D.t
  val branch: (D.t, G.t, C.t) ctx -> exp -> bool -> D.t
  val body  : (D.t, G.t, C.t) ctx -> fundec -> D.t
  val return: (D.t, G.t, C.t) ctx -> exp option  -> fundec -> D.t
  val intrpt: (D.t, G.t, C.t) ctx -> D.t
  val asm   : (D.t, G.t, C.t) ctx -> D.t
  val skip  : (D.t, G.t, C.t) ctx -> D.t


  val special : (D.t, G.t, C.t) ctx -> lval option -> varinfo -> exp list -> D.t
  val enter   : (D.t, G.t, C.t) ctx -> lval option -> fundec -> exp list -> (D.t * D.t) list
  val combine : (D.t, G.t, C.t) ctx -> lval option -> exp -> fundec -> exp list -> C.t -> D.t -> D.t

  (** Returns initial state for created thread. *)
  val threadenter : (D.t, G.t, C.t) ctx -> lval option -> varinfo -> exp list -> D.t list

  (** Updates the local state of the creator thread using initial state of created thread. *)
  val threadspawn : (D.t, G.t, C.t) ctx -> lval option -> varinfo -> exp list -> (D.t, G.t, C.t) ctx -> D.t
end

module type MCPSpec =
sig
  include Spec
  val event : (D.t, G.t, C.t) ctx -> Events.t -> (D.t, G.t, C.t) ctx -> D.t
end

type increment_data = {
  analyzed_commit_dir: string;
  current_commit_dir: string;
  changes: CompareAST.change_info
}

let empty_increment_data () = {
  analyzed_commit_dir = "";
  current_commit_dir = "";
  changes = CompareAST.empty_change_info ()
}

(** A side-effecting system. *)
module type MonSystem =
sig
  type v    (* variables *)
  type d    (* values    *)
  type 'a m (* basically a monad carrier *)

  (** Variables must be hashable, comparable, etc.  *)
  module Var : VarType with type t = v

  (** Values must form a lattice. *)
  module Dom : Lattice.S with type t = d

  (** box --- needed here for transformations *)
  val box : v -> d -> d -> d

  (** The system in functional form. *)
  val system : v -> ((v -> d) -> (v -> d -> unit) -> d) m

  (** Data used for incremental analysis *)
  val increment : increment_data
end

(** Any system of side-effecting inequations over lattices. *)
module type IneqConstrSys = MonSystem with type 'a m := 'a list

(** Any system of side-effecting equations over lattices. *)
module type EqConstrSys = MonSystem with type 'a m := 'a option

(** A side-effecting system with globals. *)
module type GlobConstrSys =
sig
  module LVar : VarType
  module GVar : VarType

  module D : Lattice.S
  module G : Lattice.S
  val increment : increment_data
  val system : LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) list
end

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key=S.v) ->
  sig
    (** The hash-map [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].  *)
    val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericIneqBoxSolver =
  functor (S: IneqConstrSys) ->
  functor (H:Hash.H with type key=S.v) ->
  sig
    (** The hash-map [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].  *)
    val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericGlobSolver =
  functor (S:GlobConstrSys) ->
  functor (LH:Hash.H with type key=S.LVar.t) ->
  functor (GH:Hash.H with type key=S.GVar.t) ->
  sig
    (** The hash-map [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].  *)
    val solve : (S.LVar.t*S.D.t) list -> (S.GVar.t*S.G.t) list -> S.LVar.t list -> S.D.t LH.t * S.G.t GH.t
  end

module ResultType2 (S:Spec) =
struct
  open S
  include Printable.Prod3 (C) (D) (Basetype.CilFundec)
  let show (es,x,f:t) = call_descr f es
  let pretty () (_,x,_) = D.pretty () x
  let printXml f (c,d,fd) =
    BatPrintf.fprintf f "<context>\n%a</context>\n%a" C.printXml c D.printXml d
end


(** Relatively safe default implementations of some boring Spec functions. *)
module DefaultSpec =
struct
  let init     () = ()
  let finalize () = ()
  (* no inits nor finalize -- only analyses like Mutex, Base, ... need
     these to do postprocessing or other imperative hacks. *)

  let should_join _ _ = true
  (* hint for path sensitivity --- MCP no longer overrides this so if you want
    your analysis to be path sensitive, do override this. To obtain a behavior
    where all paths are kept apart, set this to D.equal x y                    *)

  let call_descr f _ = f.svar.vname
  (* prettier name for equation variables --- currently base can do this and
     MCP just forwards it to Base.*)

  let intrpt x = x.local
  (* Just ignore. *)

  let vdecl ctx _ = ctx.local

  let asm x =
    ignore (M.warn "ASM statement ignored.");
    x.local (* Just ignore. *)

  let skip x = x.local (* Just ignore. *)

  let query _ (type a) (q: a Queries.t) = Queries.Result.top q
  (* Don't know anything --- most will want to redefine this. *)

  let event ctx _ _ = ctx.local

  let morphstate v d = d
  (* Only for those who track thread IDs. *)

  let sync ctx _ = ctx.local
  (* Most domains do not have a global part. *)

  let context x = x
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)

  let val_of x = x
  (* Assume that context is same as local domain. *)
end
