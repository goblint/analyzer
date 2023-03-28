(** An analysis that handles the case when an interesting function is called
    from a wrapper function all over the code. Currently handles the [malloc]-
    family of memory allocation functions, as well as [pthread_create] *)

open Prelude.Ana
open Analyses
open GobConfig
open ThreadIdDomain
module Q = Queries

(* Functor argument for creating the chain lattice of unique calls *)
module type UniqueCountArgs = sig
  val unique_count : unit -> int
  val label : string
end

(* Functor argument for determining wrapper and wrapped functions *)
module type WrapperArgs = sig
  val wrappers : unit -> string list
  val is_wrapped : LibraryDesc.special -> bool
end

(* The main analysis, generic to which functions are being wrapped. *)
module SpecBase (UniqueCountArgs : UniqueCountArgs) (WrapperArgs : WrapperArgs) =
struct
  include Analyses.DefaultSpec

  module Chain = Lattice.Chain (struct
      let n () =
        let p = UniqueCountArgs.unique_count () in
        if p < 0 then
          failwith @@ UniqueCountArgs.label ^ " has to be non-negative"
        else p + 1 (* Unique addresses + top address *)

      let names x = if x = (n () - 1) then "top" else Format.asprintf "%d" x

    end)

  (* Map for counting function call node visits up to n (of the current thread). *)
  module UniqueCallCounter = struct
    include MapDomain.MapBot_LiftTop(Q.NodeFlatLattice)(Chain)

    (* Increase counter for given node. If it does not exists yet, create it. *)
    let add_unique_call counter node =
      let unique_call = `Lifted node in
      let count = find unique_call counter in
      if Chain.is_top count then
        counter
      else
        remove unique_call counter |> add unique_call (count + 1)
  end

  module ThreadNode = struct
    include Printable.Prod3 (ThreadIdDomain.ThreadLifted) (Node) (Chain)

    (* Description that gets appended to the varinfo-name in user output. *)
    let describe_varinfo (v: varinfo) (t, node, c) =
      let loc = UpdateCil.getLoc node in
      CilType.Location.show loc

    let name_varinfo (t, node, c) =
      Format.asprintf (* TODO *) "(alloc@sid:%s@tid:%s(#%s))" (Node.show_id node) (ThreadLifted.show t) (Chain.show c)

  end

  module NodeVarinfoMap = RichVarinfo.BiVarinfoMap.Make(ThreadNode)

  module D = Lattice.Prod (UniqueCallCounter) (Q.NodeFlatLattice)
  module C = D

  let wrappers = Hashtbl.create 13

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let counter, wrapper_node = ctx.local in
    let new_wrapper_node =
      if Hashtbl.mem wrappers f.svar.vname then
        match wrapper_node with
        | `Lifted _ -> wrapper_node (* if an interesting callee is called by an interesting caller, then we remember the caller context *)
        | _         -> `Lifted ctx.node (* if an interesting callee is called by an uninteresting caller, then we remember the callee context *)
      else
        Q.NodeFlatLattice.top () (* if an uninteresting callee is called, then we forget what was called before *)
    in
    let callee = (counter, new_wrapper_node) in
    [(ctx.local, callee)]

  let combine_env ctx lval fexp f args fc (counter, _) f_ask =
    (* Keep (potentially higher) counter from callee and keep wrapper node from caller *)
    let _, lnode = ctx.local in
    (counter, lnode)

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc ((counter, _):D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special (ctx: (D.t, G.t, C.t, V.t) ctx) (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    if WrapperArgs.is_wrapped @@ desc.special arglist then
      let counter, wrapper_node = ctx.local in
      (UniqueCallCounter.add_unique_call counter ctx.node, wrapper_node)
    else ctx.local

  let startstate v = D.bot ()

  let threadenter ctx lval f args =
    (* The new thread receives a fresh counter *)
    [D.bot ()]

  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  type marshal = NodeVarinfoMap.marshal

  let init marshal =
    List.iter (fun wrapper -> Hashtbl.replace wrappers wrapper ()) (WrapperArgs.wrappers ());
    NodeVarinfoMap.unmarshal marshal

  let finalize () =
    NodeVarinfoMap.marshal ()
end


(* module UniqueCountArgsFromConfig (Option : sig val key : string end) : UniqueCountArgs = struct
  let unique_count () = get_int Option.key
  let label = "Option " ^ Option.key
end *)

(* Create the chain argument-module, given the config key to loop up *)
let unique_count_args_from_config key = (module struct
  let unique_count () = get_int key
  let label = "Option " ^ key
end : UniqueCountArgs)


module MallocWrapper : MCPSpec = struct

  include SpecBase
    (* (UniqueCountArgsFromConfig (struct let key = "ana.malloc.unique_address_count" end)) *)
    (val unique_count_args_from_config "ana.malloc.unique_address_count")
    (struct
      let wrappers () = get_string_list "ana.malloc.wrappers"

      let is_wrapped = function
      | LibraryDesc.Malloc _ | Calloc _ | Realloc _ -> true
      | _ -> false
    end)

  let name () = "mallocWrapper"

  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Q.result =
    let counter, wrapper_node = ctx.local in
    match q with
    | Q.HeapVar ->
      let node = match wrapper_node with
        | `Lifted wrapper_node -> wrapper_node
        | _ -> ctx.node
      in
      let count = UniqueCallCounter.find (`Lifted node) counter in
      let var = NodeVarinfoMap.to_varinfo (ctx.ask Q.CurrentThreadId, node, count) in
      var.vdecl <- UpdateCil.getLoc node; (* TODO: does this do anything bad for incremental? *)
      `Lifted var
    | Q.IsHeapVar v ->
      NodeVarinfoMap.mem_varinfo v
    | Q.IsMultiple v ->
      begin match NodeVarinfoMap.from_varinfo v with
        | Some (_, _, c) -> Chain.is_top c || not (ctx.ask Q.MustBeUniqueThread)
        | None -> false
      end
    | _ -> Queries.Result.top q

end


module ThreadCreateWrapper : MCPSpec = struct

  include SpecBase
    (* (UniqueCountArgsFromConfig (struct let key = "ana.thread.unique_thread_id_count" end)) *)
    (val unique_count_args_from_config "ana.thread.unique_thread_id_count")
    (struct
      let wrappers () = get_string_list "ana.thread.wrappers"

      let is_wrapped = function
      | LibraryDesc.ThreadCreate _ -> true
      | _ -> false

    end)

  let name () = "threadCreateWrapper"

  let query (ctx: (D.t, G.t, C.t, V.t) ctx) (type a) (q: a Q.t): a Q.result =
    let counter, wrapper_node = ctx.local in
    match q with
    | Q.ThreadId -> `Lifted (match wrapper_node with
      | `Lifted wrapper_node -> wrapper_node
      | _ -> ctx.node)
    | _ -> Queries.Result.top q

end

let _ = List.iter MCP.register_analysis [(module MallocWrapper); (module ThreadCreateWrapper)];
