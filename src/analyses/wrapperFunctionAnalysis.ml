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

(* The main analysis, generic to which functions are being wrapped. *)
module SpecBase (UniqueCount : Lattice.S with type t = int) (WrapperArgs : WrapperArgs) =
struct
  include IdentitySpec

  (* Use the previous CFG node (ctx.prev_node) for identifying calls to (wrapper) functions.
     For one, this is the node that typically contains the call as its statement.
     Additionally, it distinguishes two calls that share the next CFG node (ctx.node), e.g.:
     if (cond) { x = malloc(1); } else { x = malloc(2); }
     Introduce a function for this to keep things consistent. *)
  let node_for_ctx ctx = ctx.prev_node

  module NodeFlatLattice =
  struct
    include NodeFlatLattice
    let name () = "wrapper call"
  end

  module UniqueCount = UniqueCount

  (* Map for counting function call node visits up to n (of the current thread). *)
  module UniqueCallCounter =
  struct
    include MapDomain.MapBot_LiftTop(NodeFlatLattice)(UniqueCount)
    let name () = "unique calls"
  end

  (* Increase counter for given node. If it does not exist yet, create it. *)
  let add_unique_call counter node =
    let open UniqueCallCounter in
    let unique_call = `Lifted node in
    let count = find unique_call counter in
    if UniqueCount.is_top count then counter
    else remove unique_call counter |> add unique_call (count + 1)

  module D = Lattice.Prod (NodeFlatLattice) (UniqueCallCounter)
  module C = D

  let wrappers = Hashtbl.create 13

  (* transfer functions *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let wrapper_node, counter = ctx.local in
    let new_wrapper_node =
      if Hashtbl.mem wrappers f.svar.vname then
        match wrapper_node with
        (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
        | `Lifted _ -> wrapper_node
        (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
        | _         -> `Lifted (node_for_ctx ctx)
      else
        NodeFlatLattice.top () (* if an uninteresting callee is called, then we forget what was called before *)
    in
    let callee = (new_wrapper_node, counter) in
    [(ctx.local, callee)]

  let combine_env ctx lval fexp f args fc (_, counter) f_ask =
    (* Keep (potentially higher) counter from callee and keep wrapper node from caller *)
    let lnode, _ = ctx.local in
    (lnode, counter)

  let add_unique_call_ctx ctx =
    let wrapper_node, counter = ctx.local in
    wrapper_node,
    (* track the unique ID per call to the wrapper function, not to the wrapped function *)
    add_unique_call counter
      (match wrapper_node with `Lifted node -> node | _ -> node_for_ctx ctx)

  let special (ctx: (D.t, G.t, C.t, V.t) ctx) (lval: lval option) (f: varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    if WrapperArgs.is_wrapped @@ desc.special arglist then add_unique_call_ctx ctx else ctx.local

  let startstate v = D.bot ()

  let threadenter ctx ~multiple lval f args =
    (* The new thread receives a fresh counter *)
    [D.bot ()]

  let exitstate v = D.top ()

  type marshal = unit

  let init (_ : marshal option) =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (WrapperArgs.wrappers ())

end


module MallocWrapper : MCPSpec = struct

  include SpecBase
      (MallocUniqueCount)
      (struct
        let wrappers () = get_string_list "ana.malloc.wrappers"

        let is_wrapped = function
          | LibraryDesc.(Malloc _ | Calloc _ | Realloc _) -> true
          | _ -> false
      end)

  module ThreadNode = struct
    include Printable.Prod3 (ThreadIdDomain.ThreadLifted) (Node) (UniqueCount)

    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) (t, node, c) =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo (t, node, c) =
        let uniq_count =
          if not (GobConfig.get_bool "dbg.full-output") && UniqueCount.is_top c then
            Format.dprintf ""
          else
            Format.dprintf "(#%s)" (UniqueCount.show c)
        in
        let tid =
          if not (GobConfig.get_bool "dbg.full-output") && ThreadLifted.is_top t then
            Format.dprintf ""
          else
            Format.dprintf "@tid:%s%t" (ThreadLifted.show t) uniq_count
        in
        Format.asprintf "(alloc@sid:%s%t)" (Node.show_id node) tid
  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(ThreadNode)

  let name () = "mallocWrapper"

  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Q.result =
    let wrapper_node, counter = ctx.local in
    match q with
    | Q.AllocVar {on_stack = on_stack} ->
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> node_for_ctx ctx
      in
      let count = UniqueCallCounter.find (`Lifted node) counter in
      let var = NodeVarinfoMap.to_varinfo (ctx.ask Q.CurrentThreadId, node, count) in
      var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
      if on_stack then var.vattr <- addAttribute (Attr ("stack_alloca", [])) var.vattr; (* If the call was for stack allocation, add an attr to mark the heap var *)
      `Lifted var
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v && not @@ hasAttribute "stack_alloca" v.vattr
    | Q.IsAllocVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      begin match NodeVarinfoMap.from_varinfo v with
        | Some (_, _, c) -> UniqueCount.is_top c || not (ctx.ask Q.MustBeUniqueThread)
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


module ThreadCreateWrapper : MCPSpec = struct

  include SpecBase
      (ThreadCreateUniqueCount)
      (struct
        let wrappers () = get_string_list "ana.thread.wrappers"

        let is_wrapped = function
          | LibraryDesc.ThreadCreate _ -> true
          | _ -> false

      end)

  let name () = "threadCreateWrapper"

  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.ThreadCreateIndexedNode ->
      let wrapper_node, counter = ctx.local in
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> node_for_ctx ctx
      in
      let count = UniqueCallCounter.find (`Lifted node) counter in
      `Lifted node, count
    | _ -> Queries.Result.top q

end

let _ = List.iter MCP.register_analysis [(module MallocWrapper); (module ThreadCreateWrapper)]
