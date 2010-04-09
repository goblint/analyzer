open Cil
open Pretty

module GU = Goblintutil
module M  = Messages

(** General type of an analyzer. *)
module type S =
sig
  val name: string
  (** name of the analyzer*)
  val analyze: file -> fundec list -> unit
  (** analyze a file -- output using Messages.* *)
end

type local_state = [ 
    | `Base        of BaseDomain.Dom(ConcDomain.Simple).t
    | `Mutex       of LockDomain.Lockset.t
    | `SymbLocks   of LockDomain.Symbolic.t
    | `VarEq       of PartitionDomain.ExpPartitions.t
    | `Uninit      of ValueDomain.AddrSetDomain.t
    | `Malloc_null of ValueDomain.AddrSetDomain.t
    | `Thread      of ConcDomain.Simple.t
    | `Escape      of SetDomain.HeadlessSet (Basetype.Variables).t
    | `Region      of RegionDomain.RegionDom.t
    | `OSEK        of LockDomain.Lockset.t
    | `Access      of AccessDomain.Access.t
    ]

module Context 
  (Dom:  Lattice.S)
  (Glob: Global.S) =
struct
  type ctx = 
    { query : Queries.t -> Queries.Result.t
    ; local : Dom.t
    ; global: Glob.Var.t -> Glob.Val.t 
    ; deps  : local_state list                }
  
end

type ('a,'b,'c) ctx = 
    { ask   : Queries.t -> Queries.Result.t
    ; local : 'a
    ; global: 'b -> 'c 
    ; sub   : local_state list                }

let set_q ctx ask =
  {ask = ask; local=ctx.local; global=ctx.global;sub=ctx.sub}

let set_st ctx st =
  {ask = ctx.ask; local=st; global=ctx.global;sub=ctx.sub}

let set_gl ctx gl =
  {ask = ctx.ask; local=ctx.local; global=gl;sub=ctx.sub}

let set_st_gl ctx st gl =
  {ask = ctx.ask; local=st; global=gl;sub=ctx.sub}

let context ask st gl dp = {ask=ask; local=st; global=gl;sub=dp}


module type VarType = 
sig
  include Hashtbl.HashedType
  val pretty_trace: unit -> t -> doc
  val compare : t -> t -> int
end


module type Spec = 
sig
  module Dom : Lattice.S   
  (** THE data structure *)
  module Glob : Global.S
  (** global variable and value type*)    
  
    
  val name: string
  (** name of the analysis *)
  val init: unit -> unit
  (** first function to be called when analyzing using this Spec *)
  val finalize: unit -> unit
  (** last function to be called when analyzing using this Spec *)
  
  val should_join: Dom.t -> Dom.t -> bool
  (** sensitivity predicate *)
  val startstate: unit -> Dom.t
  (** state to start analyzing the main function*)
  val otherstate: unit -> Dom.t
  (** state to start analyzing other functions (usual when calling './goblint --allfuns ...') *)
  val es_to_string: fundec -> Dom.t -> string
  (** no-one knows .. somehow used when generating final output *)
  
  val reset_diff : Dom.t -> Dom.t
  (** resets the global difference part of the state *)
  val get_diff : Dom.t -> (Glob.Var.t * Glob.Val.t) list
  (** returns global differences from state *)
  
  
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
    
                 |-> enter_func -> <analyze the functions> -> leave_func -> join -> ...
    eval_funvar -|-> special_fn ----------------------------------------------^
                 |-> fork  ------------------------------------------------------->
  *)
  
  
  val eval_funvar: (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> exp -> varinfo list
  (** [eval_funvar q f st] evaluates [f] to a list of possible functions (in state [st]) *)
  val fork       : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (varinfo * Dom.t) list  
  (** [fork] returns list of function,input-state pairs, that the callee has spawned *)
  val special_fn : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (Dom.t * Cil.exp * bool) list
  (** [special_fn] is called, when given varinfo is not connected to a fundec -- no function definition is given*)
  val enter_func : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> (Dom.t * Dom.t) list 
  (** [enter_func] returns input-states that must be analyzed for the given function *)
  val leave_func : (Dom.t, Glob.Var.t, Glob.Val.t) ctx -> lval option -> varinfo -> exp list -> Dom.t -> Dom.t
  (** [leave_func q lv f a x y] does postprocessing on the analyzed [enter_func q lv f a x] output [y] -- usually readding some
     context from [x] *)

end


module VarF (LD: Printable.S) =  
struct
  type t = MyCFG.node * LD.t

  let hash x = 
    match x with
      | (MyCFG.Statement s,d) -> Hashtbl.hash (d, s.sid, 0)
      | (MyCFG.Function  f,d) -> Hashtbl.hash (d, f.vid, 1)

  let equal (n1,d1) (n2,d2) = MyCFG.Node.equal n1 n2 && LD.equal d1 d2
      
  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () x =
    match x with
      | (MyCFG.Statement s,d) -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
      | (MyCFG.Function  f,d) -> dprintf "call of %s" f.vname
                
  let pretty_trace () x = dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x)

  let compare (n1,d1) (n2,d2) =
    let comp =
    match n1, n2 with
      | MyCFG.Statement _, MyCFG.Function _  -> -1
      | MyCFG.Function  _, MyCFG.Statement _ -> 1
      | MyCFG.Statement s, MyCFG.Statement l -> compare s.sid l.sid
      | MyCFG.Function  f, MyCFG.Function g  -> compare f.vid g.vid
    in
    if comp == 0 then LD.compare d1 d2 else comp
end

module VarCS =
struct
  type t = MyCFG.node * location

  let hash (n,l) = 
    match n with
      | MyCFG.Statement s -> Hashtbl.hash (l, s.sid, 0)
      | MyCFG.Function f -> Hashtbl.hash (l, f.vid, 1)

  let equal (n1,d1) (n2,d2) =
    MyCFG.Node.equal n1 n2 && compareLoc d1 d1 = 0

  let getLocation (n,d) = MyCFG.getLoc n

  let pretty () (n,d) =
    match n with
      | MyCFG.Statement s -> dprintf "node \"%a\"" Basetype.CilStmt.pretty s
      | MyCFG.Function f -> dprintf "call of %s" f.vname

  let pretty_trace () x = 
    dprintf "%a on %a" pretty x Basetype.ProgLines.pretty (getLocation x)
end

exception Deadcode


(** [Dom (D)] produces D lifted where bottom means dead-code *)
module Dom (LD: Lattice.S) = 
struct 
  include Lattice.Lift (LD) (struct
                               let bot_name = "Dead code"
                               let top_name = "Totally unknown & messed up"
                             end)
  let lift x = `Lifted x

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
  let toXML (_,x,_ as st) = 
    let esc = Goblintutil.escape in
      match SD.toXML x with 
	| Xml.Element (tag, attr, children) ->
            Xml.Element (tag, [("text", esc (short 80 st))], children)
	| x -> x
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

  let output table =
    let out = M.get_out result_name !GU.out in
    match !GU.result_style with
      | GU.Pretty -> ignore (fprintf out "%a\n" pretty table)
      | GU.Indented -> begin
          Xmldump.print_fmt out (resultXML table);
          output_char out '\n'
        end
      | GU.Compact -> begin
          Xmldump.print out (resultXML table);
          output_char out '\n'
        end
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
