(** Control Flow Graphs. *)
open Cil

type node = 
  | Statement of stmt  
  (** The statements as identified by CIL *)
  | FunctionEntry of varinfo
  (** *)
  | Function of varinfo  
  (** The variable information associated with the function declaration. *)
(** A node in the Control Flow Graph is either a statement or function. Think of
  * the function node as last node that all the returning nodes point to.  So
  * the result of the function call is contained in the fucntion node. *)

val pretty_node : unit -> node -> Pretty.doc

module Node : Hashtbl.HashedType with type t = node
(** The HashedType module for nodes *)

type asm_out = (string option * string * lval) list
type asm_in  = (string option * string * exp ) list
(** Types for the inline assembly. *)

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
    * transfered to the function node! *)
  | Test of exp * bool 
  (** The true-branch or false-branch of a conditional exp *) 
  | ASM of string list * asm_out * asm_in
  (** Inline assembly statements, and the annotations for output and input
    * variables. *)
  | Skip 
  (** This is here for historical reasons. I never use Skip edges! *)
  | SelfLoop 
  (** This for interrupt edges.! *)

val pretty_edge : unit -> edge -> Pretty.doc

type cfg = node -> (edge * node) list
(** The control flow graph is a function that for each node returns the set of
  * edges entering the node and the node each edge originated from. This will
  * only work for forward analyses! *)

val unknown_exp: exp
(** An unknown valued expression. *)
val dummy_func: fundec
(** An additional function added to the CFG. As of now it only has a direct 
  * edge (Return) to its entry.*)

val getCFG: file -> cfg
(** Returns the cfg of the given AST. Note that if the variable
  * {!Goblinutil.cfg_print} is set (by the flag "--cfg"), this function will
  * also write to the file cfg.dot *)

val getGlobalInits: file -> (edge * location) list
(** Returns a list of globals and their initializer expressions as [Assign]
  * edges.  We rely on {!Cil.foldLeftCompoundAll} to initialize complicated
  * structures and handle missing initializers.Hopefully, the user doesn't need
  * to worry about initializing globals. *)

val getLoc: node -> location
(** Returns the source location of the given node. For a function, this is the
  * point were the function is defined. *)

val getFun: node -> fundec
(** Returns the function that the given node belongs to. *)

val initfun: fundec
(** All the global initializers are put into this function, so that the assign
  * transfer function will take care of initializers in a uniform way... very
  * nice! *)
