(** Current thread ID analysis ([threadid]). *)

module LF = LibraryFunctions

open Batteries
open Analyses
open GobList.Syntax

module Thread = ThreadIdDomain.Thread
module ThreadLifted = ThreadIdDomain.ThreadLifted

let get_current (ask: Queries.ask): ThreadLifted.t =
  ask.f Queries.CurrentThreadId

let get_current_unlift ask: Thread.t =
  match get_current ask with
  | `Lifted thread -> thread
  | _ -> failwith "ThreadId.get_current_unlift"

module VNI =
  Printable.Prod3
    (CilType.Varinfo)
    (Node) (
    Printable.Option
      (WrapperFunctionAnalysis0.ThreadCreateUniqueCount)
      (struct let name = "no index" end))

module Spec =
struct
  include Analyses.IdentitySpec

  module N =
  struct
    include Lattice.FlatConf (struct include Printable.DefaultConf let bot_name = "unknown node" let top_name = "unknown node" end) (VNI)
    let name () = "wrapper call"
  end
  module TD = Thread.D
  module Created =
  struct
    module Current =
    struct
      include TD
      let name () = "current function"
    end
    module Callees =
    struct
      include TD
      let name () = "callees"
    end
    include Lattice.Prod (Current) (Callees)
    let name () = "created"
  end

  (** Uniqueness Counter * TID * (All thread creates of current thread * All thread creates of the current function and its callees) *)
  module D = Lattice.Prod3 (N) (ThreadLifted) (Created)
  module C = D
  module P = IdentityP (D)

  let tids = ref (Hashtbl.create 20)

  let name () = "threadid"

  let context ctx fd ((n,current,td) as d) =
    if GobConfig.get_bool "ana.thread.context.create-edges" then
      d
    else
      (n, current, (TD.bot (), TD.bot ()))

  let startstate v = (N.bot (), ThreadLifted.bot (), (TD.bot (),TD.bot ()))
  let exitstate  v = (N.bot (), `Lifted (Thread.threadinit v ~multiple:false), (TD.bot (), TD.bot ()))

  let morphstate v _ =
    let tid = Thread.threadinit v ~multiple:false in
    if GobConfig.get_bool "dbg.print_tids" then
      Hashtbl.replace !tids tid ();
    (N.bot (), `Lifted (tid), (TD.bot (), TD.bot ()))

  let create_tid ?(multiple=false) (_, current, (td, _)) ((node, index): Node.t * int option) v =
    match current with
    | `Lifted current ->
      let+ tid = Thread.threadenter ~multiple (current, td) node index v in
      if GobConfig.get_bool "dbg.print_tids" then
        Hashtbl.replace !tids tid ();
      `Lifted tid
    | _ ->
      [`Lifted (Thread.threadinit v ~multiple:true)]

  let is_unique ctx =
    ctx.ask Queries.MustBeUniqueThread

  let enter ctx lval f args =
    let (n, current, (td, _)) = ctx.local in
    [ctx.local, (n, current, (td,TD.bot ()))]

  let combine_env ctx lval fexp f args fc ((n,current,(_, au_ftd)) as au) f_ask =
    let (_, _, (td, ftd)) = ctx.local in
    if not (GobConfig.get_bool "ana.thread.context.create-edges") then
      (n,current,(TD.join td au_ftd, TD.join ftd au_ftd))
    else
      au

  let created (_, current, (td, _)) =
    match current with
    | `Lifted current -> BatOption.map_default (ConcDomain.ThreadSet.of_list) (ConcDomain.ThreadSet.top ()) (Thread.created current td)
    | _ -> ConcDomain.ThreadSet.top ()

  let query (ctx: (D.t, _, _, _) ctx) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.CurrentThreadId -> Tuple3.second ctx.local
    | Queries.CreatedThreads -> created ctx.local
    | Queries.MustBeUniqueThread ->
      begin match Tuple3.second ctx.local with
        | `Lifted tid -> Thread.is_unique tid
        | _ -> Queries.MustBool.top ()
      end
    | Queries.MustBeSingleThreaded {since_start} ->
      begin match Tuple3.second ctx.local with
        | `Lifted tid when Thread.is_main tid ->
          let created = created ctx.local in
          if since_start then
            ConcDomain.ThreadSet.is_empty created
          else if ctx.ask Queries.ThreadsJoinedCleanly then
            let joined = ctx.ask Queries.MustJoinedThreads in
            ConcDomain.ThreadSet.is_empty (ConcDomain.ThreadSet.diff created joined)
          else
            false
        | _ -> false
      end
    | _ -> Queries.Result.top x

  module A =
  struct
    include Printable.Option (ThreadLifted) (struct let name = "nonunique" end)
    let name () = "thread"
    let may_race (t1: t) (t2: t) = match t1, t2 with
      | Some t1, Some t2 when ThreadLifted.equal t1 t2 -> false (* only unique threads *)
      | _, _ -> true
    let should_print = Option.is_some
  end

  let access ctx _ =
    if is_unique ctx then
      let tid = Tuple3.second ctx.local in
      Some tid
    else
      None

  (** get the node that identifies the current context, possibly that of a wrapper function *)
  let indexed_node_for_ctx ctx =
    match ctx.ask Queries.ThreadCreateIndexedNode with
    | `Lifted node, count when WrapperFunctionAnalysis.ThreadCreateUniqueCount.is_top count -> node, None
    | `Lifted node, count -> node, Some count
    | (`Bot | `Top), _ -> ctx.prev_node, None

  let threadenter ctx ~multiple lval f args:D.t list =
    let n, i = indexed_node_for_ctx ctx in
    let+ tid = create_tid ~multiple ctx.local (n, i) f in
    (`Lifted (f, n, i), tid, (TD.bot (), TD.bot ()))

  let threadspawn ctx ~multiple lval f args fctx =
    let (current_n, current, (td,tdl)) = ctx.local in
    let v, n, i = match fctx.local with `Lifted vni, _, _ -> vni | _ -> failwith "ThreadId.threadspawn" in
    (current_n, current, (Thread.threadspawn ~multiple td n i v, Thread.threadspawn ~multiple tdl n i v))

  type marshal = (Thread.t,unit) Hashtbl.t (* TODO: don't use polymorphic Hashtbl *)
  let init (m:marshal option): unit =
    match m with
    | Some y -> tids := y
    | None -> ()


  let print_tid_info () =
    let tids = Hashtbl.to_list !tids in
    let uniques = List.filter_map (fun (a,b) -> if Thread.is_unique a then Some a else None) tids in
    let non_uniques = List.filter_map (fun (a,b) -> if not (Thread.is_unique a) then Some a else None) tids in
    let uc = List.length uniques in
    let nc = List.length non_uniques in
    M.debug_noloc ~category:Analyzer "Encountered number of thread IDs (unique): %i (%i)" (uc+nc) uc;
    M.msg_group Debug ~category:Analyzer "Unique TIDs" (List.map (fun tid -> (Thread.pretty () tid, None)) uniques);
    M.msg_group Debug ~category:Analyzer "Non-unique TIDs" (List.map (fun tid -> (Thread.pretty () tid, None)) non_uniques)

  let finalize () =
    if GobConfig.get_bool "dbg.print_tids" then print_tid_info ();
    !tids
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
