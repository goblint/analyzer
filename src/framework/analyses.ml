(** Signatures for aanlyzers, analysis specifications, and result output.  *)

open Cil
open Pretty
open GobConfig

module GU = Goblintutil
module M  = Messages

(** Analysis starts from lists of functions: start functions, exit functions, and
  * other functions. *)
type fundecs = fundec list * fundec list * fundec list

(** General type of an analyzer. *)
module type S =
sig
  val name: string
  (** name of the analyzer*)
  val analyze: file -> fundecs -> unit
  (** analyze a file -- output using Messages.* *)
end

type local_state = [ 
    | `Base        of BaseDomain.Dom.t
    | `Mutex       of LockDomain.Lockset.t
    | `SymbLocks   of LockDomain.Symbolic.t
    | `VarEq       of PartitionDomain.ExpPartitions.t
    | `Uninit      of ValueDomain.AddrSetDomain.t
    | `Malloc_null of ValueDomain.AddrSetDomain.t
    | `Thread      of ConcDomain.ThreadDomain.t
    | `Escape      of EscapeDomain.EscapedVars.t
    | `File        of FileDomain.FileUses.t
    | `Spec        of SpecDomain.Dom.t
    | `Region      of RegionDomain.RegionDom.t
    | `OSEK        of LockDomain.Lockset.t
    | `OSEK2       of Osektupel.t*Osektupel.t
    | `OSEK3       of IntDomain.Flattened.t
(*    | `Access      of AccessDomain.Access.t*)
    | `Contain     of ContainDomain.Dom.t
    | `Shape       of ShapeDomain.Dom.t*RegionDomain.RegionDom.t
    | `Stack       of StackDomain.Dom.t
    | `Stack2       of StackDomain.Dom2.t
    | `Stack3       of StackDomain.Dom3.t
    | `Flag        of ConcDomain.Trivial.t
    | `MayLocks    of LockDomain.MayLockset.t
    | `ThreadLocSet of ConcDomain.ThreadStringSet.t
    | `Oslo         of LockDomain.Lockset.t
    | `FlagModeDom  of FlagModeDomain.Dom.t
    | `Deadlock    of DeadlockDomain.Lockset.t
    | `Unit
    ]

type global_state = [
    | `Base   of BaseDomain.Glob.Val.t
    | `Mutex  of LockDomain.Glob.Val.t
    | `Oslo  of LockDomain.Glob.Val.t
    | `Osek  of LockDomain.OsekGlob.Val.t
    | `Region of RegionDomain.RegPart.t
(*    | `Access of AccessDomain.Access.GlobDom.t *)
    | `Contain of ContainDomain.Globals.t
    | `Shapes of ShapeDomain.Bool.t * RegionDomain.RegPart.t
    | `None ]

(* Experiment to reduce the number of arguments on transfer functions and allow
  sub-analyses. The list sub contains the current local states of analyses in
  the same order as writen in the dependencies list (in MCP).
  
  The foreign states when calling special_fn or enter are joined if the foreign 
  analysis tries to be path-sensitive in these functions. First try to only
  depend on simple analyses.
 
  It is not clear if we need pre-states, post-states or both on foreign analyses.
 *)
type ('a,'b,'c) ctx = 
    { ask   : Queries.t -> Queries.Result.t
    ; local : 'a
    ; global: 'b -> 'c 
    ; presub: local_state list
    ; sub   : local_state list
    ; spawn : varinfo -> 'a -> unit
    ; geffect : 'b -> 'c -> unit 
    ; precomp : local_state list list 
    ; preglob : (varinfo -> global_state list) list 
    ; report_access : [ `Lval of lval * bool | `Reach of exp * bool ] -> unit
    }

let set_q ctx ask = 
  {ctx with ask = ask} 

let set_st ctx st spawn_tr =
  {ctx with local=st; spawn=spawn_tr ctx.spawn}

let swap_st ctx st =
  {ctx with local=st}

let set_gl ctx gl eff_tr =
  {ctx with global=gl; geffect=eff_tr ctx.geffect}

let set_st_gl ctx st gl spawn_tr eff_tr =
  {ctx with local=st; global=gl; spawn=spawn_tr ctx.spawn; geffect=eff_tr ctx.geffect}

let set_precomp ctx pc = 
  {ctx with precomp = pc}

let set_preglob ctx pg = 
  {ctx with preglob = pg}

let context ask st gl dp sp ge rep = {ask=ask; local=st; global=gl;sub=dp;presub=[];spawn=sp;geffect=ge;precomp=[];preglob=[]; report_access=rep}

module type DomainTranslator =
sig
  type from_type
  type to_type
  
  val translate : from_type -> to_type 
end

module type VarType = 
sig
  include Hashtbl.HashedType
  val pretty_trace: unit -> t -> doc
  val compare : t -> t -> int
  val category : t -> int
  
  val line_nr : t -> int
  val file_name : t -> string
  val description : t -> string
  val context : unit -> t -> doc
  val loopSep : t -> bool
end

module type Spec = 
sig
  module Dom : Lattice.S   
  (** THE data structure *)
  module Glob : Glob.S
  (** global variable and value type*)    
  
    
  val name: string
  (** name of the analysis *)
  val init: unit -> unit
  (** first function to be called when analyzing using this Spec *)
  val finalize: unit -> unit
  (** last function to be called when analyzing using this Spec *)
  
  val context_top: varinfo -> Dom.t -> Dom.t
  (** Keeps only context sensitive part, set rest to top. *)
  val should_join: Dom.t -> Dom.t -> bool
  (** sensitivity predicate *)
  val startstate: varinfo -> Dom.t
  (** state to start analyzing the start functions *)
  val morphstate: varinfo -> Dom.t -> Dom.t
  (** Change the starting state after initialization code for the start functions *)
  val exitstate: varinfo -> Dom.t
  (** state to start analyzing the exit functions *)
  val otherstate: varinfo -> Dom.t
  (** state to start analyzing other functions (usual when calling './goblint --allfuns ...') *)
  val es_to_string: fundec -> Dom.t -> string
  (** no-one knows .. somehow used when generating final output *)
  val may_race: ((Dom.t, Glob.Var.t, Glob.Val.t) ctx * [ `Lval of lval * bool | `Reach of exp * bool ]) 
                -> ((Dom.t, Glob.Var.t, Glob.Val.t) ctx * [ `Lval of lval * bool | `Reach of exp * bool ]) -> bool 
  (** query if two accesses may conflict *)
  
  val sync: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> Dom.t * (Glob.Var.t * Glob.Val.t) list
  (** Synchronize with the global invariant. This is applied after joining with
    * the previous state, see test 02/04 for an example why this is needed. *)
  
  (** Query function: *)
  val query: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> Queries.t -> Queries.Result.t 
  (** Answers our ... ahem ... queries. *)

  (** Transfer functions:  *)
  
  val assign: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval -> exp -> Dom.t 
  (** handle assignments *)
  val branch: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> exp -> bool -> Dom.t
  (** handle branches *)
  val body  : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> fundec -> Dom.t
  (** steping inside of a function body *)
  val return: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> exp option  -> fundec -> Dom.t
  (** steping out from a function *)
  

  (** Transfer function for function calls: *)
  
  (* Basic scheme:
    |----------------------------------------------v
    |-> enter_func -> <analyze the functions> -> leave_func -> join -> ...
    |-> special_fn ----------------------------------------------^
    |-> fork  ------------------------------------------------------->
  *)
  
  
(*   val fork       : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (varinfo * Dom.t) list   *)
(*   (** [fork] returns list of function,input-state pairs, that the callee has spawned *) *)
  val special_fn : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (Dom.t * Cil.exp * bool) list
  (** [special_fn] is called, when given varinfo is not connected to a fundec -- no function definition is given*)
  val enter_func : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (Dom.t * Dom.t) list 
  (** [enter_func] returns input-states that must be analyzed for the given function *)
  val leave_func : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> exp -> varinfo -> exp list -> Dom.t -> Dom.t
  (** [leave_func q lv f a x y] does postprocessing on the analyzed [enter_func q lv f a x] output [y] -- usually readding some
     context from [x] *)

  val intrpt: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> Dom.t
  (** Transfer function for interrupts. *)
end

module StatsTrace (D : Spec) = 
struct
  let time_f n f =
    let padr n s : string = 
      let len = String.length s in
      if len > n then s else 
        let res = String.create n in
        String.blit s 0 res 0 len;
        String.fill res len (n-len) ' '; 
        res
    in      
    if (get_bool "dbg.debug") then Stats.time (padr 40 (D.name^" ⇢ "^n)) f
        else f
  
  module Dom = 
  struct
    include D.Dom
    
    let hash x = time_f "Dom ↣ hash" hash x
    let equal x y = time_f "Dom ↣ equal" (equal x) y
    let join x y = time_f "Dom ↣ join" (join x) y
    let meet x y = time_f "Dom ↣ meet" (meet x) y
    let leq x y = time_f "Dom ↣ leq" (leq x) y
  end       

  module Glob = D.Glob
    
  let name         = D.name
  let init         = D.init
  let finalize     = D.finalize
  let context_top  = D.context_top
  let should_join  = D.should_join
  let startstate   = D.startstate
  let morphstate   = D.morphstate
  let otherstate   = D.otherstate
  let exitstate    = D.exitstate
  let es_to_string = D.es_to_string
  let sync         = D.sync
  let query        = D.query 
  let assign       = D.assign
  let branch       = D.branch
  let body         = D.body  
  let return       = D.return
  let special_fn   = D.special_fn 
  let enter_func   = D.enter_func 
  let leave_func   = D.leave_func 
  let intrpt       = D.intrpt
  
  
  (* transfer functions *)
  let may_race x = time_f "may_race" (D.may_race x)
  let sync ctx  = time_f "sync" sync ctx  
  let should_join x y  = time_f "should_join" (should_join x) y
  let query ctx q  = time_f "query" (query ctx) q
  let assign ctx lval rval = time_f "assign" (assign ctx lval) rval  
  let branch ctx exp tv = time_f "branch" (branch ctx exp) tv 
  let body ctx fundec = time_f "body" (body ctx) fundec
  let return ctx exp f = time_f "return" (return ctx exp) f  
  let enter_func ctx lval f args = time_f "enter_func" (enter_func ctx lval f) args  
  let leave_func ctx lval fexp f args au = time_f "leave_func" (leave_func ctx lval fexp f args) au  
  let special_fn ctx lval f arglist = time_f "special_fn" (special_fn ctx lval f) arglist

end

module HCLift (D : Spec) = 
struct
  module Dom = Lattice.HConsed (D.Dom)
  module Glob = D.Glob
  
  let lift_f1 f x = Dom.lift (f (Dom.unlift x))
  let lift_f2 f x y = Dom.lift (f (Dom.unlift x) (Dom.unlift y))
  let lift_f2s f x y = f (Dom.unlift x) (Dom.unlift y)
  let lift_f1s f x = f (Dom.unlift x)
  let lift_fc f x = f (set_st x (Dom.unlift x.local) (fun sp f x -> sp f (Dom.lift x)))
  let name         = D.name
  let init         = D.init
  let finalize     = D.finalize
  let context_top f = lift_f1 (D.context_top f)
  let should_join  = lift_f2s D.should_join
  let startstate   v = Dom.lift (D.startstate v)
  let morphstate   v = lift_f1  (D.morphstate v)
  let otherstate   v = Dom.lift (D.otherstate v)
  let exitstate    v = Dom.lift (D.exitstate  v)
  let es_to_string f = lift_f1s (D.es_to_string f)
  let sync x        = let x, y = lift_fc D.sync x in Dom.lift x, y
  let query        = lift_fc D.query 
  let assign c l r = Dom.lift (lift_fc D.assign c l r)
  let branch c e t = Dom.lift (lift_fc D.branch c e t)
  let body c f     = Dom.lift (lift_fc D.body c f)  
  let return c e f = Dom.lift (lift_fc D.return c e f)
  let special_fn c r f xs = List.map (fun (x,y,z) -> Dom.lift x, y, z) (lift_fc D.special_fn c r f xs) 
  let enter_func c r f xs = List.map (fun (x,y) -> Dom.lift x, Dom.lift y) (lift_fc D.enter_func c r f xs)
  let leave_func c r fe f xs d  = Dom.lift (lift_fc D.leave_func c r fe f xs (Dom.unlift d))
  let intrpt c     = Dom.lift (lift_fc D.intrpt c)
end


(** Relatively safe default implementations of some boring Spec functions. *)
module DefaultSpec =
struct
  let init     () = ()
  let finalize () = ()
  (* no inits nor finalize -- only analyses like Mutex, Base, ... need 
     these to do postprocessing or other imperative hacks. *)
  
  let should_join _ _ = true
  (* hint for path sensitivity --- MCP overrides this so don't we don't bother. *)
  
  let es_to_string f _ = f.svar.vname
  (* prettier name for equation variables --- currently base can do this and
     MCP just forwards it to Base.*)
  
  let context_top f x = x
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)
  
  let sync ctx     = (ctx.local,[])
  (* Most domains do not have a global part. *)
  
  let query _ (q:Queries.t) = Queries.Result.top ()
  (* Don't know anything --- most will want to redefine this. *)
  
  let may_race _ _ = true
  (* Don't know anything --- may lead to a race *)
  
  let eval_funvar _ _ = []
  (* Only base analysis should know this. *)

  let intrpt x = x.local
  (* Just ignore. *)

  let morphstate v d = d
  (* Only for those who track thread IDs. *)
end

module Var =  
struct
  type t = MyCFG.node

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

  let line_nr n = (MyCFG.getLoc n).line 
  let file_name n = (MyCFG.getLoc n).file
  let description n = sprint 80 (pretty () n)
  let context () _ = Pretty.nil
  let loopSep =  function 
    | MyCFG.Statement s -> MyCFG.loopSep s
    | _ -> false
end


module VarF (LD: Printable.S) =  
struct
  type t = MyCFG.node * LD.t

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
                
  let pretty_trace () x =
      match x with
      | ((*MyCFG.FunctionEntry f*)_,d) -> dprintf "%a" pretty x 
(*       | _ -> dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x) *)


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
    
  let line_nr (n,_) = (MyCFG.getLoc n).line 
  let file_name (n,_) = (MyCFG.getLoc n).file
  let description (n,_) = sprint 80 (Var.pretty () n)
  let context () (_,c) = LD.pretty () c
  let loopSep =  function 
    | MyCFG.Statement s, _ -> MyCFG.loopSep s
    | MyCFG.FunctionEntry _, _ -> true
    | MyCFG.Function _, _ -> false
end

module VarCS =
struct
  type t = MyCFG.node * location

  let hash (n,l) = 
    match n with
      | MyCFG.Statement s -> Hashtbl.hash (l, s.sid, 0)
      | MyCFG.Function f -> Hashtbl.hash (l, f.vid, 1)
      | MyCFG.FunctionEntry f -> Hashtbl.hash (l, f.vid, 2)

  let equal (n1,d1) (n2,d2) =
    MyCFG.Node.equal n1 n2 && compareLoc d1 d1 = 0

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () (n,d) =
    match n with
      | MyCFG.Statement s -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
      | MyCFG.Function f -> dprintf "call of %s" f.vname
      | MyCFG.FunctionEntry f -> dprintf "entry state of %s" f.vname

  let pretty_trace () x = 
    dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x)
end

module Edge : Hashtbl.HashedType with type t = MyCFG.node * MyCFG.edge * MyCFG.node =
struct
  type t = MyCFG.node * MyCFG.edge * MyCFG.node
  let rec list_eq eq xs ys =
    match xs, ys with 
      | [], [] -> true
      | x::xs, y::ys when eq x y -> list_eq eq xs ys
      | _ -> false

  let eq_lval l1 l2 = Util.equals (Lval l1) (Lval l2)
       
  open MyCFG 
  let eq_edge e1 e2 =
    match e1, e2 with
      | Assign (l1,e1), Assign (l2,e2) -> Util.equals e1 e2 && eq_lval l1 l2
      | Proc (Some l1, e1, es1), Proc (Some l2, e2, es2) -> eq_lval l1 l2 && Util.equals e1 e2 && list_eq Util.equals es1 es2
      | Proc (None, e1, es1), Proc (None, e2, es2) -> Util.equals e1 e2 && list_eq Util.equals es1 es2
      | Entry f1, Entry f2 -> f1.svar.vid = f2.svar.vid
      | Ret (Some e1,f1), Ret (Some e2,f2)-> Util.equals e1 e2 && f1.svar.vid = f2.svar.vid
      | Ret (None,f1), Ret (None,f2) -> f1.svar.vid = f2.svar.vid       
      | Test (e1,b1), Test (e2,b2) -> b1 = b2 && Util.equals e1 e2
      | ASM (s1,o1,i1), ASM (s2,o2,i2) -> s1 = s2
      | Skip, Skip -> true
      | SelfLoop, SelfLoop -> true
      | _ -> false
	let equal (f1,e1,t1) (f2,e2,t2) = MyCFG.Node.equal f1 f2 && MyCFG.Node.equal t1 t2 && eq_edge e1 e2
  let hash (f,e,t) = MyCFG.Node.hash f lxor MyCFG.Node.hash t
end

exception Deadcode


(** [Dom (D)] produces D lifted where bottom means dead-code *)
module Dom (LD: Lattice.S) = 
struct 
  include Lattice.Lift (LD) (struct
                               let bot_name = "Dead code"
                               let top_name = "Totally unknown & messed up"
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
end


module ResultType (Spec: Spec) (LD: Printable.S with type t = Spec.Dom.t) (SD: Printable.S) = 
struct
  include Printable.Prod3 (LD) (SD) (Basetype.CilFundec)
  let isSimple _ = false
  let short w (es,x,f) = Spec.es_to_string f es
  let toXML (es,x,_ as st) = 
    let open Xml in
    let flatten_single = function
      | Element (_,_,[x]) | x ->  x in
    let try_replace_text s = function
    	| Element (tag, attr, children) -> Element (tag, ["text", s], children) 
    	| x -> x
    in
    let esc = Goblintutil.escape in
    let ctx = try_replace_text "Context" (flatten_single (Spec.Dom.toXML es)) in
    let res = try_replace_text "Value" (flatten_single (SD.toXML x)) in
      Element ("Node",["text",esc (short 80 st)],[ctx;res])            
  let pretty () (_,x,_) = SD.pretty () x
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

  let output table gtable (file: Cil.file) =
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
          Xmldump.print out (resultXML (Lazy.force table));
          output_char out '\n'
        end
      | "html" -> 
          Htmldump.print_html out (resultXML (Lazy.force table)) file gtable
      | _ -> ()
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

(*********************)

type ('d,'g) ctx2 = 
    { ask2      : Queries.t -> Queries.Result.t
    ; local2    : 'd
    ; global2   : varinfo -> 'g 
    ; presub2   : (string * Obj.t) list
    ; postsub2  : (string * Obj.t) list
    ; spawn2    : varinfo -> 'd -> unit
    ; split2    : 'd -> exp -> bool -> unit
    ; sideg2    : varinfo -> 'g -> unit 
    }

module type Spec2 =
sig
  module D : Lattice.S
  module G : Lattice.S
  module C : Printable.S
  
  val name : string
  
  val init : unit -> unit
  val finalize : unit -> unit
  
  val startstate : varinfo -> D.t
  val morphstate : varinfo -> D.t -> D.t
  val exitstate  : varinfo -> D.t
  val otherstate : varinfo -> D.t

  val should_join : D.t -> D.t -> bool
  val context : D.t -> C.t
  val call_descr : fundec -> C.t -> string
  
  val sync  : (D.t, G.t) ctx2 -> D.t * (varinfo * G.t) list
  val query : (D.t, G.t) ctx2 -> Queries.t -> Queries.Result.t 
  val assign: (D.t, G.t) ctx2 -> lval -> exp -> D.t 
  val branch: (D.t, G.t) ctx2 -> exp -> bool -> D.t
  val body  : (D.t, G.t) ctx2 -> fundec -> D.t
  val return: (D.t, G.t) ctx2 -> exp option  -> fundec -> D.t
  val intrpt: (D.t, G.t) ctx2 -> D.t
  

  val special : (D.t, G.t) ctx2 -> lval option -> varinfo -> exp list -> D.t
  val enter   : (D.t, G.t) ctx2 -> lval option -> varinfo -> exp list -> (D.t * D.t) list 
  val combine : (D.t, G.t) ctx2 -> lval option -> exp -> varinfo -> exp list -> D.t -> D.t
end

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
module type GenericGlobSolver =
  functor (S:GlobConstrSys) ->
  functor (LH:Hash.H with type key=S.LVar.t) ->
  functor (GH:Hash.H with type key=S.GVar.t) ->
sig
  (** The hash-map [solve box xs vs] is a local solution for interesting variables [vs],
      reached from starting values [xs].  *)
  val solve : (S.LVar.t*S.D.t) list -> (S.GVar.t*S.G.t) list -> S.LVar.t list -> S.D.t LH.t * S.G.t GH.t
end

module ResultType2 (S:Spec2) = 
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
end
