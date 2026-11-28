(** Family of analyses which provide symbolic locations for special library functions.
    Provides symbolic heap locations for dynamic memory allocations and symbolic thread
    identifiers for thread creation ([mallocWrapper], [threadCreateWrapper]).

    Provided heap locations are based on the node and thread ID.
    Provided thread identifiers are based solely the node.
    Considers wrapper functions and a number of unique heap locations
    or thread identifiers for additional precision. *)

open GoblintCil
open Analyses
open GobConfig
open ThreadIdDomain
module Q = Queries

include WrapperFunctionAnalysis0

(* Functor argument for determining wrapper and wrapped functions *)
module type WrapperArgs = sig
  val wrappers : unit -> string list
  val is_wrapped : LibraryDesc.special -> bool
end

module type CallCounter = sig
  include Lattice.S
  val initial : unit -> t
  val increment : Node.t -> t -> t
end

module CountMap (Count : Lattice.S with type t = int) = struct
  include MapDomain.MapBot_LiftTop(NodeFlatLattice)(Count)
  let initial = bot
  let increment node counter =
    let key = `Lifted node in
    let count = find key counter in
    if Count.is_top count then counter else add key (count + 1) counter
end

module MallocCounter = struct
  module Bound = CountMap (MallocUniqueCount)
  (* Intervals ordered by inclusion. Missing entries in both maps are zero,
     so joining a visited and an unvisited site retains zero as a possibility. *)
  include Lattice.Prod (Lattice.Reverse (Bound)) (Bound)
  let initial () = Bound.initial (), Bound.initial ()
  let increment node (lo, hi) = Bound.increment node lo, Bound.increment node hi
  let bounds node (lo, hi) = Bound.find (`Lifted node) lo, Bound.find (`Lifted node) hi
end

module ThreadCounter = CountMap (ThreadCreateUniqueCount)

(* The main analysis, generic to which functions are being wrapped. *)
module SpecBase (Counter : CallCounter) (WrapperArgs : WrapperArgs) =
struct
  include IdentitySpec

  (* Use the previous CFG node (man.prev_node) for identifying calls to (wrapper) functions.
     For one, this is the node that typically contains the call as its statement.
     Additionally, it distinguishes two calls that share the next CFG node (man.node), e.g.:
     if (cond) { x = malloc(1); } else { x = malloc(2); }
     Introduce a function for this to keep things consistent. *)
  let node_for_man man = man.prev_node

  module NodeFlatLattice =
  struct
    include NodeFlatLattice
    let name () = "wrapper call"
  end

  module WrapperArgs = WrapperArgs

  (* Map for counting function call node visits up to n (of the current thread). *)
  module UniqueCallCounter =
  struct
    include Counter
    let name () = "unique calls"
  end

  module D = Lattice.Prod (NodeFlatLattice) (UniqueCallCounter)
  include Analyses.ValueContexts(D)

  let wrappers = Hashtbl.create 13

  (* transfer functions *)

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let wrapper_node, counter = man.local in
    let new_wrapper_node =
      if Hashtbl.mem wrappers f.svar.vname then
        match wrapper_node with
        (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
        | `Lifted _ -> wrapper_node
        (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
        | _         -> `Lifted (node_for_man man)
      else
        NodeFlatLattice.top () (* if an uninteresting callee is called, then we forget what was called before *)
    in
    let callee = (new_wrapper_node, counter) in
    [(man.local, callee)]

  let combine_env man lval fexp f args fc (_, counter) f_ask =
    (* Keep the updated counter from the callee and the wrapper node from the caller. *)
    let lnode, _ = man.local in
    (lnode, counter)

  let add_unique_call_man man =
    let wrapper_node, counter = man.local in
    wrapper_node,
    (* track the unique ID per call to the wrapper function, not to the wrapped function *)
    Counter.increment (match wrapper_node with `Lifted node -> node | _ -> node_for_man man) counter

  let special (man: (D.t, G.t, C.t, V.t) man) (lval: lval option) (f: varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    if WrapperArgs.is_wrapped @@ desc.special arglist then add_unique_call_man man else man.local

  let startstate v = NodeFlatLattice.bot (), Counter.initial ()

  let threadenter man ~multiple lval f args =
    (* The new thread receives a fresh counter *)
    [startstate f]

  let exitstate v = D.top ()

  type marshal = unit

  let init (_ : marshal option) =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (WrapperArgs.wrappers ())

end


module MallocWrapper = struct

  include SpecBase
      (MallocCounter)
      (struct
        let wrappers () = get_string_list "ana.malloc.wrappers"

        let is_wrapped = function
          | LibraryDesc.(Malloc _ | Calloc _ | Realloc _) -> true (* No Alloca because alloca variables are out of scope and invalid when returned from a wrapper anyway. *)
          | _ -> false
      end)

  module ThreadNode = struct
    include Printable.Prod3 (ThreadIdDomain.ThreadLifted) (Node) (MallocUniqueCount)

    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) (t, node, c) =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo (t, node, c) =
      let uniq_count =
        if not (GobConfig.get_bool "dbg.full-output") && MallocUniqueCount.is_top c then
          Format.dprintf ""
        else
          Format.dprintf "(#%s)" (MallocUniqueCount.show c)
      in
      let tid =
        if not (GobConfig.get_bool "dbg.full-output") && ThreadLifted.is_top t then
          Format.dprintf ""
        else
          Format.dprintf "@tid:%s" (ThreadLifted.show t)
      in
      Format.asprintf "(alloc@sid:%s%t%t)" (Node.show_id node) tid uniq_count

    let typ _ = GoblintCil.voidType
  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(ThreadNode)

  let name () = "mallocWrapper"

  let query (man: (D.t, G.t, C.t, V.t) man) (type a) (q: a Q.t): a Q.result =
    let wrapper_node, counter = man.local in
    match q with
    | Q.AllocVar location ->
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> node_for_man man
      in
      let lo, hi = MallocCounter.bounds node counter in
      let tid = man.ask Q.CurrentThreadId in
      let vars = ref (Q.VS.empty ()) in
      for count = lo to hi do
        let var = NodeVarinfoMap.to_varinfo (tid, node, count) in
        var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
        if location = Stack then var.vattr <- addAttribute (Attr ("stack_alloca", [])) var.vattr;
        vars := Q.VS.add var !vars
      done;
      !vars
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v && not @@ hasAttribute "stack_alloca" v.vattr
    | Q.IsAllocVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      begin match NodeVarinfoMap.from_varinfo v with
        | Some (t, _, c) -> MallocUniqueCount.is_top c ||
                            (match t with
                             | `Lifted tid -> not (Thread.is_unique tid)
                             | _ ->
                               (* The thread analysis may be completely disabled; in this case we fall back on checking whether the program has been single threaded since start *)
                               ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man)
                            )
        | None -> false
      end
    | _ -> Queries.Result.top q

  type marshal = NodeVarinfoMap.marshal

  let init marshal =
    (* call init from SpecBase *)
    init None;
    NodeVarinfoMap.unmarshal marshal

  let finalize () =
    NodeVarinfoMap.marshal ()
end


module ThreadCreateWrapper = struct

  include SpecBase
      (ThreadCounter)
      (struct
        let wrappers () = get_string_list "ana.thread.wrappers"

        let is_wrapped = function
          | LibraryDesc.ThreadCreate _ -> true
          | _ -> false

      end)

  let name () = "threadCreateWrapper"

  let query (man: (D.t, G.t, C.t, V.t) man) (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.ThreadCreateIndexedNode ->
      let wrapper_node, counter = man.local in
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> node_for_man man
      in
      let count = ThreadCounter.find (`Lifted node) counter in
      `Lifted node, count
    | _ -> Queries.Result.top q

end

let _ = List.iter MCP.register_analysis [(module MallocWrapper); (module ThreadCreateWrapper)]
