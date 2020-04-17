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
  type t = MyCFG.node
  let relift x = x

  let category = function
    | MyCFG.Statement     s -> 1
    | MyCFG.Function      f -> 2
    | MyCFG.FunctionEntry f -> 3

  let hash x =
    match x with
    | MyCFG.Statement     s -> Hashtbl.hash (s.sid, 0)
    | MyCFG.Function      f -> Hashtbl.hash (f.vid, 1)
    | MyCFG.FunctionEntry f -> Hashtbl.hash (f.vid, 2)

  let equal = MyCFG.Node.equal

  let getLocation n = MyCFG.getLoc n

  let pretty () x =
    match x with
    | MyCFG.Statement     s -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
    | MyCFG.Function      f -> dprintf "call of %s" f.vname
    | MyCFG.FunctionEntry f -> dprintf "entry state of %s" f.vname

  let pretty_trace () x =  dprintf "%a on %a \n" pretty x Basetype.ProgLines.pretty (getLocation x)

  let compare n1 n2 =
    match n1, n2 with
    | MyCFG.FunctionEntry f, MyCFG.FunctionEntry g -> compare f.vid g.vid
    | _                    , MyCFG.FunctionEntry g -> -1
    | MyCFG.FunctionEntry g, _                     -> 1
    | MyCFG.Statement _, MyCFG.Function _  -> -1
    | MyCFG.Function  _, MyCFG.Statement _ -> 1
    | MyCFG.Statement s, MyCFG.Statement l -> compare s.sid l.sid
    | MyCFG.Function  f, MyCFG.Function g  -> compare f.vid g.vid

  let kind = function
    | MyCFG.Function f                         -> `ExitOfProc f
    | MyCFG.Statement {skind = Instr [Call _]} -> `ProcCall
    | _ -> `Other

  let printXml f n =
    let id ch n =
      match n with
      | MyCFG.Statement s     -> BatPrintf.fprintf ch "%d" s.sid
      | MyCFG.Function f      -> BatPrintf.fprintf ch "ret%d" f.vid
      | MyCFG.FunctionEntry f -> BatPrintf.fprintf ch "fun%d" f.vid
    in
    let l = MyCFG.getLoc n in
    BatPrintf.fprintf f "<call id=\"%a\" file=\"%s\" fun=\"%s\" line=\"%d\" order=\"%d\">\n" id n l.file (MyCFG.getFun n).svar.vname l.line l.byte

  let var_id n =
    match n with
    | MyCFG.Statement s     -> string_of_int s.sid
    | MyCFG.Function f      -> "ret" ^ string_of_int f.vid
    | MyCFG.FunctionEntry f -> "fun" ^ string_of_int f.vid

  let line_nr n = (MyCFG.getLoc n).line
  let file_name n = (MyCFG.getLoc n).file
  let description n = sprint 80 (pretty () n)
  let context () _ = Pretty.nil
  let node n = n
end


module VarF (LD: Printable.HC) =
struct
  type t = MyCFG.node * LD.t
  let relift (n,x) = n, LD.relift x

  let category = function
    | (MyCFG.Statement     s,_) -> 1
    | (MyCFG.Function      f,_) -> 2
    | (MyCFG.FunctionEntry f,_) -> 3

  let hashmul x y = if x=0 then y else if y=0 then x else x*y
  let hash x =
    match x with
    | (MyCFG.Statement     s,d) -> hashmul (LD.hash d) (s.sid*17)
    | (MyCFG.Function      f,d) -> hashmul (LD.hash d) (f.vid*19)
    | (MyCFG.FunctionEntry f,d) -> hashmul (LD.hash d) (f.vid*23)

  let equal (n1,d1) (n2,d2) = MyCFG.Node.equal n1 n2 && LD.equal d1 d2

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () x =
    match x with
    | (MyCFG.Statement     s,d) -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
    | (MyCFG.Function      f,d) -> dprintf "call of %s" f.vname
    | (MyCFG.FunctionEntry f,d) -> dprintf "entry state of %s" f.vname

  let pretty_trace () (n,c as x) =
    if get_bool "dbg.trace.context" then dprintf "(%a, %a) on %a \n" pretty x LD.pretty c Basetype.ProgLines.pretty (getLocation x)
    else dprintf "%a on %a \n" pretty x Basetype.ProgLines.pretty (getLocation x)

  let compare (n1,d1) (n2,d2) =
    let comp =
      match n1, n2 with
      | MyCFG.FunctionEntry f, MyCFG.FunctionEntry g -> compare f.vid g.vid
      | _                    , MyCFG.FunctionEntry g -> -1
      | MyCFG.FunctionEntry g, _                     -> 1
      | MyCFG.Statement _, MyCFG.Function _  -> -1
      | MyCFG.Function  _, MyCFG.Statement _ -> 1
      | MyCFG.Statement s, MyCFG.Statement l -> compare s.sid l.sid
      | MyCFG.Function  f, MyCFG.Function g  -> compare f.vid g.vid
    in
    if comp == 0 then LD.compare d1 d2 else comp

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
    | `Top -> BatPrintf.fprintf f "<value>%s</value>" (Goblintutil.escape top_name)
    | `Bot -> ()
    | `Lifted x -> LD.printXml f x
end


open Xml

module type ResultConf =
sig
  val result_name: string
end

module type RS =
sig
  include Printable.S
  include ResultConf
  type key = Basetype.ProgLinesFun.t
  type value
  val create: int -> t
  val clear: t -> unit
  val copy: t -> t
  val add: t -> key -> value -> unit
  val remove: t -> key -> unit
  val find: t -> key -> value
  val find_all: t -> key -> value list
  val replace : t -> key -> value -> unit
  val mem : t -> key -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val fold: (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val length: t -> int

  val resultXML: t -> Xml.xml
  val output: t -> unit
end

module Result (Range: Printable.S) (C: ResultConf) =
struct
  include Hash.Printable (Basetype.ProgLinesFun) (Range)
  include C

  let toXML x =
    let full_result = toXML x in
    let fatten_maps  (o:xml list) (x:xml) :xml list =
      match x with
      | Xml.Element (_,_,child) -> child @ o
      | z -> z::o in

    let group_loc_ch x =
      match x with
      | Xml.Element ("Loc",b,c) -> Xml.Element ("Loc",b,List.fold_left fatten_maps [] c)
      | z -> z in

    match full_result with
    | Xml.Element (_,_,child) ->
      Xml.Element (result_name, [("name", "Who cares?")],
                   List.map group_loc_ch child)
    | _ -> failwith "Empty analysis?"

  let resultXML x = toXML x

  let printXml f xs =
    let print_id f = function
      | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
      | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.vid
      | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.vid
    in
    let print_one (loc,n,fd) v =
      BatPrintf.fprintf f "<call id=\"%a\" file=\"%s\" line=\"%d\" order=\"%d\">\n" print_id n loc.file loc.line loc.byte;
      BatPrintf.fprintf f "%a</call>\n" Range.printXml v
    in
    iter print_one xs

  let printJson f xs =
    let print_id f = function
      | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
      | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.vid
      | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.vid
    in
    let print_one (loc,n,fd) v =
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

  let output table gtable gtxml gtfxml (file: file) =
    if (get_bool "dbg.verbose") then print_endline ("Filtering output for files that match : '"^ (!GU.result_filter)^"'");
    GU.result_regexp := (Str.regexp (!GU.result_filter));
    let out = Messages.get_out result_name !GU.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
    | "indented" -> begin
        Xmldump.print_fmt out (resultXML (Lazy.force table));
        output_char out '\n'
      end
    | "compact" -> begin
        if (get_bool "dbg.verbose") then Printf.printf "Converting to xml.%!";
        let xml = resultXML (Lazy.force table) in
        if (get_bool "dbg.verbose") then Printf.printf "Printing the result.%!";
        Xmldump.print out xml;
        output_char out '\n'
      end
    | "html" ->
      Htmldump.print_html out (resultXML (Lazy.force table)) file (lazy ((gtxml gtable) :: []))
    | "fast_xml" ->
      let module SH = BatHashtbl.Make (Basetype.RawStrings) in
      let file2funs = SH.create 100 in
      let funs2node = SH.create 100 in
      iter (fun (_,n,_) _ -> SH.add funs2node (MyCFG.getFun n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_node f = function
        | MyCFG.Statement stmt  -> BatPrintf.fprintf f "%d" stmt.sid
        | MyCFG.Function g      -> BatPrintf.fprintf f "ret%d" g.vid
        | MyCFG.FunctionEntry g -> BatPrintf.fprintf f "fun%d" g.vid
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
        BatPrintf.fprintf f "<run><parameters>%a</parameters><result>\n" (BatArray.print ~first:"" ~last:"" ~sep:" " BatString.print) BatSys.argv;
        BatEnum.iter (fun b -> BatPrintf.fprintf f "<file name=\"%s\" path=\"%s\">\n%a</file>\n" (Filename.basename b) b p_funs (SH.find_all file2funs b)) (SH.keys file2funs);
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
      iter (fun (_,n,_) _ -> SH.add funs2node (MyCFG.getFun n).svar.vname n) (Lazy.force table);
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
        | MyCFG.Function g      -> fprintf f "\"ret%d\"" g.vid
        | MyCFG.FunctionEntry g -> fprintf f "\"fun%d\"" g.vid
      in
      let p_fun f x = fprintf f "{\n  \"name: \"%s\",\n  \"nodes\": %a\n}" x (p_list p_node) (SH.find_all funs2node x) in
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
   the same order as writen in the dependencies list (in MCP).

   The foreign states when calling special_fn or enter are joined if the foreign
   analysis tries to be path-sensitive in these functions. First try to only
   depend on simple analyses.

   It is not clear if we need pre-states, post-states or both on foreign analyses.
*)
type ('d,'g,'c) ctx =
  { ask      : Queries.t -> Queries.Result.t
  ; node     : MyCFG.node
  ; prev_node: MyCFG.node
  ; context  : Obj.t (** represented type: unit -> (Control.get_spec ()).C.t *)
  ; context2 : unit -> 'c
  ; edge     : MyCFG.edge
  ; local    : 'd
  ; global   : varinfo -> 'g
  ; presub   : (string * Obj.t) list
  ; postsub  : (string * Obj.t) list
  ; spawn    : varinfo -> 'd -> unit
  ; split    : 'd -> exp -> bool -> unit
  ; sideg    : varinfo -> 'g -> unit
  ; assign   : ?name:string -> lval -> exp -> unit
  }

exception Ctx_failure of string
(** Failure from ctx, e.g. global initializer *)

let ctx_failwith s = raise (Ctx_failure s) (* TODO: use everywhere in ctx *)

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
  val otherstate : varinfo -> D.t

  val should_join : D.t -> D.t -> bool
  val val_of  : C.t -> D.t
  val context : D.t -> C.t
  val call_descr : fundec -> C.t -> string
  val part_access: (D.t, G.t, C.t) ctx -> exp -> varinfo option -> bool -> (Access.LSSSet.t * Access.LSSet.t)

  val sync  : (D.t, G.t, C.t) ctx -> D.t * (varinfo * G.t) list
  val query : (D.t, G.t, C.t) ctx -> Queries.t -> Queries.Result.t
  val assign: (D.t, G.t, C.t) ctx -> lval -> exp -> D.t
  val branch: (D.t, G.t, C.t) ctx -> exp -> bool -> D.t
  val body  : (D.t, G.t, C.t) ctx -> fundec -> D.t
  val return: (D.t, G.t, C.t) ctx -> exp option  -> fundec -> D.t
  val intrpt: (D.t, G.t, C.t) ctx -> D.t
  val asm   : (D.t, G.t, C.t) ctx -> D.t
  val skip  : (D.t, G.t, C.t) ctx -> D.t


  val special : (D.t, G.t, C.t) ctx -> lval option -> varinfo -> exp list -> D.t
  val enter   : (D.t, G.t, C.t) ctx -> lval option -> varinfo -> exp list -> (D.t * D.t) list
  val combine : (D.t, G.t, C.t) ctx -> lval option -> exp -> varinfo -> exp list -> C.t -> D.t -> D.t
end

module type SpecHC = (* same as Spec but with relift function for hashcons in context module *)
sig
  module C : Printable.HC
  include Spec with module C := C
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
  type v    (** variables *)
  type d    (** values    *)
  type 'a m (** basically a monad carrier *)

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
  let isSimple _ = false
  let short w (es,x,f:t) = call_descr f es
  let toXML (es,x,_ as st:t) =
    let open Xml in
    let flatten_single = function
      | Element (_,_,[x]) | x ->  x in
    let try_replace_text s = function
      | Element (tag, attr, children) -> Element (tag, ["text", s], children)
      | x -> x
    in
    let esc = Goblintutil.escape in
    let ctx = try_replace_text "Context" (flatten_single (C.toXML es)) in
    let res = try_replace_text "Value" (flatten_single (D.toXML x)) in
    Element ("Node",["text",esc (short 80 st)],[ctx;res])
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

  let asm x =
    ignore (M.warn "ASM statement ignored.");
    x.local (* Just ignore. *)

  let skip x = x.local (* Just ignore. *)

  let query _ (q:Queries.t) = Queries.Result.top ()
  (* Don't know anything --- most will want to redefine this. *)

  let morphstate v d = d
  (* Only for those who track thread IDs. *)

  let sync ctx     = (ctx.local,[])
  (* Most domains do not have a global part. *)

  let context x = x
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)

  let val_of x = x
  (* Assume that context is same as local domain. *)

  let part_access _ _ _ _ =
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
    (* No partitioning on accesses and not locks *)
end
