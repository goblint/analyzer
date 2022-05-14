(** Signatures for analyzers, analysis specifications, and result output.  *)

open Prelude
open Cil
open Pretty
open GobConfig

module GU = Goblintutil
module M  = Messages

let currentFunctionName: string ref = ref ""

(** Analysis starts from lists of functions: start functions, exit functions, and
  * other functions. *)
type fundecs = fundec list * fundec list * fundec list

module type VarType =
sig
  include Hashtbl.HashedType
  val pretty_trace: unit -> t -> doc
  val compare : t -> t -> int

  val printXml : 'a BatInnerIO.output -> t -> unit
  val var_id   : t -> string
  val node      : t -> MyCFG.node
  val relift    : t -> t (* needed only for incremental+hashcons to re-hashcons contexts after loading *)
end

module Var =
struct
  type t = Node.t [@@deriving eq, ord, hash]
  let relift x = x

  let getLocation n = Node.location n

  let pretty_trace () x =  dprintf "%a on %a" Node.pretty_trace x CilType.Location.pretty (getLocation x)

  let printXml f n =
    let l = Node.location n in
    BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" fun=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\">\n" (Node.show_id n) l.file (Node.find_fundec n).svar.vname l.line l.byte l.column

  let var_id = Node.show_id
  let node n = n
end


module VarF (LD: Printable.S) =
struct
  type t = Node.t * LD.t [@@deriving eq, ord, hash]
  let relift (n,x) = n, LD.relift x

  let getLocation (n,d) = Node.location n

  let pretty_trace () ((n,c) as x) =
    if get_bool "dbg.trace.context" then dprintf "(%a, %a) on %a \n" Node.pretty_trace n LD.pretty c CilType.Location.pretty (getLocation x)
    else dprintf "%a on %a" Node.pretty_trace n CilType.Location.pretty (getLocation x)

  let printXml f (n,c) =
    Var.printXml f n;
    BatPrintf.fprintf f "<context>\n";
    LD.printXml f c;
    BatPrintf.fprintf f "</context>\n"

  let var_id (n,_) = Var.var_id n
  let node (n,_) = n
end

module GVarF (V: Printable.S) =
struct
  include V
  (* from Basetype.Variables *)
  let var_id _ = "globals"
  let node _ = MyCFG.Function Cil.dummyFunDec
  let pretty_trace = pretty
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


module ResultNode: Printable.S with type t = MyCFG.node =
struct
  include Printable.Std

  include Node

  let name () = "resultnode"

  let show a =
    (* Not using Node.location here to have updated locations in incremental analysis.
       See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let x = UpdateCil.getLoc a in
    let f = Node.find_fundec a in
    CilType.Location.show x ^ "(" ^ f.svar.vname ^ ")"

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module type ResultConf =
sig
  val result_name: string
end

module Result (Range: Printable.S) (C: ResultConf) =
struct
  include Hashtbl.Make (ResultNode)
  type nonrec t = Range.t t (* specialize polymorphic type for Range values *)

  let pretty () mapping =
    let f key st dok =
      dok ++ dprintf "%a ->@?  @[%a@]\n" ResultNode.pretty key Range.pretty st
    in
    let content () = fold f mapping nil in
    let defline () = dprintf "OTHERS -> Not available\n" in
    dprintf "@[Mapping {\n  @[%t%t@]}@]" content defline

  include C

  let printXml f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in

      let parentNode = Node.find_fundec n in
      currentFunctionName.contents <- parentNode.svar.vname;

      BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\">\n" (Node.show_id n) loc.file loc.line loc.byte loc.column;
      BatPrintf.fprintf f "%a</call>\n" Range.printXml v
    in
    iter print_one xs

  let printJson f xs =
    let print_one n v =
      (* Not using Node.location here to have updated locations in incremental analysis.
         See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      BatPrintf.fprintf f "{\n\"id\": \"%s\", \"file\": \"%s\", \"line\": \"%d\", \"byte\": \"%d\", \"column\": \"%d\", \"states\": %s\n},\n" (Node.show_id n) loc.file loc.line loc.byte loc.column (Yojson.Safe.to_string (Range.to_yojson v))
    in
    iter print_one xs

  let printXmlWarning f () =
    let one_text f Messages.Piece.{loc; text = m; _} =
      match loc with
      | Some l ->
        BatPrintf.fprintf f "\n<text file=\"%s\" line=\"%d\" column=\"%d\">%s</text>" l.file l.line l.column (GU.escape m)
      | None ->
        () (* TODO: not outputting warning without location *)
    in
    let one_w f (m: Messages.Message.t) = match m.multipiece with
      | Single piece  -> one_text f piece
      | Group {group_text = n; pieces = e} ->
        BatPrintf.fprintf f "<group name=\"%s\">%a</group>\n" n (BatList.print ~first:"" ~last:"" ~sep:"" one_text) e
    in
    let one_w f x = BatPrintf.fprintf f "\n<warning>%a</warning>" one_w x in
    List.iter (one_w f) !Messages.Table.messages_list

  let output table gtable gtfxml (file: file) =
    let out = Messages.get_out result_name !GU.out in
    match get_string "result" with
    | "pretty" -> ignore (fprintf out "%a\n" pretty (Lazy.force table))
    | "fast_xml" ->
      let module SH = BatHashtbl.Make (Basetype.RawStrings) in
      let file2funs = SH.create 100 in
      let funs2node = SH.create 100 in
      iter (fun n _ -> SH.add funs2node (Node.find_fundec n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_node f n = BatPrintf.fprintf f "%s" (Node.show_id n) in
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
        Stdlib.close_out chn;
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
      iter (fun n _ -> SH.add funs2node (Node.find_fundec n).svar.vname n) (Lazy.force table);
      iterGlobals file (function
          | GFun (fd,loc) -> SH.add file2funs loc.file fd.svar.vname
          | _ -> ()
        );
      let p_enum p f xs = BatEnum.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      let p_list p f xs = BatList.print ~first:"[\n  " ~last:"\n]" ~sep:",\n  " p f xs in
      (*let p_kv f (k,p,v) = fprintf f "\"%s\": %a" k p v in*)
      (*let p_obj f xs = BatList.print ~first:"{\n  " ~last:"\n}" ~sep:",\n  " p_kv xs in*)
      let p_node f n = BatPrintf.fprintf f "\"%s\"" (Node.show_id n) in
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
    | "sarif" ->
      let open BatPrintf in
      printf "Writing Sarif to file: %s\n%!" (get_string "outfile");
      Yojson.Safe.pretty_to_channel ~std:true out (Sarif.to_yojson (List.rev !Messages.Table.messages_list));
    | "json-messages" ->
      let files = Hashtbl.to_list Preprocessor.dependencies in
      let filter_system = List.filter_map (fun (f,system) -> if system then None else Some f) in
      let json = `Assoc [
          ("files", `Assoc (List.map (Tuple2.map2 (fun deps -> [%to_yojson:string list] @@ filter_system deps)) files));
          ("messages", Messages.Table.to_yojson ());
        ]
      in
      Yojson.Safe.pretty_to_channel ~std:true out json
    | "none" -> ()
    | s -> failwith @@ "Unsupported value for option `result`: "^s
end


(* Experiment to reduce the number of arguments on transfer functions and allow
   sub-analyses. The list sub contains the current local states of analyses in
   the same order as written in the dependencies list (in MCP).

   The foreign states when calling special_fn or enter are joined if the foreign
   analysis tries to be path-sensitive in these functions. First try to only
   depend on simple analyses.

   It is not clear if we need pre-states, post-states or both on foreign analyses.
*)
type ('d,'g,'c,'v) ctx =
  { ask      : 'a. 'a Queries.t -> 'a Queries.result (* Inlined Queries.ask *)
  ; emit     : Events.t -> unit
  ; node     : MyCFG.node
  ; prev_node: MyCFG.node
  ; control_context : Obj.t (** (Control.get_spec ()) context, represented type: unit -> (Control.get_spec ()).C.t *)
  ; context  : unit -> 'c (** current Spec context *)
  ; edge     : MyCFG.edge
  ; local    : 'd
  ; global   : 'v -> 'g
  ; presub   : string -> Obj.t (** raises [Not_found] if such dependency analysis doesn't exist *)
  ; postsub  : string -> Obj.t (** raises [Not_found] if such dependency analysis doesn't exist *)
  ; spawn    : lval option -> varinfo -> exp list -> unit
  ; split    : 'd -> Events.t list -> unit
  ; sideg    : 'v -> 'g -> unit
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
  module V: Printable.S (** Global constraint variables. *)

  val name : unit -> string

  (** Auxiliary data (outside of solution domains) that needs to be marshaled and unmarshaled.
      This includes:
      * hashtables,
      * varinfos (create_var),
      * RichVarinfos. *)
  type marshal

  (** Initialize using unmarshaled auxiliary data (if present). *)
  val init : marshal option -> unit

  (** Finalize and return auxiliary data to be marshaled. *)
  val finalize : unit -> marshal
  (* val finalize : G.t -> unit *)

  val startstate : varinfo -> D.t
  val morphstate : varinfo -> D.t -> D.t
  val exitstate  : varinfo -> D.t

  val should_join : D.t -> D.t -> bool
  val context : fundec -> D.t -> C.t
  val call_descr : fundec -> C.t -> string

  val sync  : (D.t, G.t, C.t, V.t) ctx -> [`Normal | `Join | `Return] -> D.t
  val query : (D.t, G.t, C.t, V.t) ctx -> 'a Queries.t -> 'a Queries.result
  val assign: (D.t, G.t, C.t, V.t) ctx -> lval -> exp -> D.t
  val vdecl : (D.t, G.t, C.t, V.t) ctx -> varinfo -> D.t
  val branch: (D.t, G.t, C.t, V.t) ctx -> exp -> bool -> D.t
  val body  : (D.t, G.t, C.t, V.t) ctx -> fundec -> D.t
  val return: (D.t, G.t, C.t, V.t) ctx -> exp option  -> fundec -> D.t
  val intrpt: (D.t, G.t, C.t, V.t) ctx -> D.t
  val asm   : (D.t, G.t, C.t, V.t) ctx -> D.t
  val skip  : (D.t, G.t, C.t, V.t) ctx -> D.t


  val special : (D.t, G.t, C.t, V.t) ctx -> lval option -> varinfo -> exp list -> D.t
  val enter   : (D.t, G.t, C.t, V.t) ctx -> lval option -> fundec -> exp list -> (D.t * D.t) list
  val combine : (D.t, G.t, C.t, V.t) ctx -> lval option -> exp -> fundec -> exp list -> C.t option -> D.t -> D.t

  (** Returns initial state for created thread. *)
  val threadenter : (D.t, G.t, C.t, V.t) ctx -> lval option -> varinfo -> exp list -> D.t list

  (** Updates the local state of the creator thread using initial state of created thread. *)
  val threadspawn : (D.t, G.t, C.t, V.t) ctx -> lval option -> varinfo -> exp list -> (D.t, G.t, C.t, V.t) ctx -> D.t
end

module type MCPA =
sig
  include Printable.S
  val may_race: t -> t -> bool
  val should_print: t -> bool (** Whether value should be printed in race output. *)
end

module type MCPSpec =
sig
  include Spec
  val event : (D.t, G.t, C.t, V.t) ctx -> Events.t -> (D.t, G.t, C.t, V.t) ctx -> D.t

  module A: MCPA
  val access: (D.t, G.t, C.t, V.t) ctx -> exp -> varinfo option -> bool -> A.t
end

type analyzed_data = {
  cil_file: Cil.file ;
  solver_data: Obj.t;
}

type increment_data = {
  old_data: analyzed_data option;
  new_file: Cil.file;
  changes: CompareCIL.change_info
}

let empty_increment_data file = {
  old_data = None;
  new_file = file;
  changes = CompareCIL.empty_change_info ()
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
  val system : LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) option
end

(** A solver is something that can translate a system into a solution (hash-table).
    Incremental solver has data to be marshaled. *)
module type GenericEqBoxIncrSolverBase =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    type marshal

    (** The hash-map that is the first component of [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].
        As a second component the solver returns data structures for incremental serialization. *)
    val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t * marshal
  end

(** (Incremental) solver argument, indicating which postsolving should be performed by the solver. *)
module type IncrSolverArg =
sig
  val should_prune: bool
  val should_verify: bool
  val should_warn: bool
  val should_save_run: bool
end

(** An incremental solver takes the argument about postsolving. *)
module type GenericEqBoxIncrSolver =
  functor (Arg: IncrSolverArg) ->
    GenericEqBoxIncrSolverBase

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    (** The hash-map that is the first component of [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs]. *)
    val solve : (S.v -> S.d -> S.d -> S.d) -> (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericGlobSolver =
  functor (S:GlobConstrSys) ->
  functor (LH:Hashtbl.S with type key=S.LVar.t) ->
  functor (GH:Hashtbl.S with type key=S.GVar.t) ->
  sig
    type marshal

    (** The hash-map that is the first component of [solve box xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].
        As a second component the solver returns data structures for incremental serialization. *)
    val solve : (S.LVar.t*S.D.t) list -> (S.GVar.t*S.G.t) list -> S.LVar.t list -> (S.D.t LH.t * S.G.t GH.t) * marshal
  end

module ResultType2 (S:Spec) =
struct
  open S
  include Printable.Prod3 (C) (D) (CilType.Fundec)
  let show (es,x,f:t) = call_descr f es
  let pretty () (_,x,_) = D.pretty () x
  let printXml f (c,d,fd) =
    BatPrintf.fprintf f "<context>\n%a</context>\n%a" C.printXml c D.printXml d
end

module VarinfoV = CilType.Varinfo (* TODO: or Basetype.Variables? *)
module EmptyV = Printable.Empty

module UnitA =
struct
  include Printable.Unit
  let may_race _ _ = true
  let should_print _ = false
end


(** Relatively safe default implementations of some boring Spec functions. *)
module DefaultSpec =
struct
  module G = Lattice.Unit
  module V = EmptyV

  type marshal = unit
  let init _ = ()
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
    ignore (M.info ~category:Unsound "ASM statement ignored.");
    x.local (* Just ignore. *)

  let skip x = x.local (* Just ignore. *)

  let query _ (type a) (q: a Queries.t) = Queries.Result.top q
  (* Don't know anything --- most will want to redefine this. *)

  let event ctx _ _ = ctx.local

  let morphstate v d = d
  (* Only for those who track thread IDs. *)

  let sync ctx _ = ctx.local
  (* Most domains do not have a global part. *)

  let context fd x = x
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)

  module A = UnitA
  let access _ _ _ _ = ()
end

(* Even more default implementations. Most transfer functions acting as identity functions. *)
module IdentitySpec =
struct
  include DefaultSpec
  let assign ctx (lval:lval) (rval:exp) =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) =
    ctx.local

  let body ctx (f:fundec) =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc au =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) =
    ctx.local

  let threadenter ctx lval f args = [ctx.local]
  let threadspawn ctx lval f args fctx = ctx.local
end
