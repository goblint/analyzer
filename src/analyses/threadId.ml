(** Current thread ID analysis. *)

module LF = LibraryFunctions

open Batteries
open GoblintCil
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


module Spec =
struct
  include Analyses.IdentitySpec

  module TD = Thread.D

  module D = Lattice.Prod (ThreadLifted) (TD)
  module C = D

  let tids = ref (Hashtbl.create 20)

  let name () = "threadid"

  let startstate v = (ThreadLifted.bot (), TD.bot ())
  let exitstate  v = (`Lifted (Thread.threadinit v ~multiple:false), TD.bot ())

  let morphstate v _ =
    let tid = Thread.threadinit v ~multiple:false in
    if GobConfig.get_bool "dbg.print_tids" then
      Hashtbl.replace !tids tid ();
    (`Lifted (tid), TD.bot ())

  let create_tid (current, td) (node: Node.t) v =
    match current with
    | `Lifted current ->
      let+ tid = Thread.threadenter (current, td) node v in
      if GobConfig.get_bool "dbg.print_tids" then
        Hashtbl.replace !tids tid ();
      `Lifted tid
    | _ ->
      [`Lifted (Thread.threadinit v ~multiple:true)]

  let is_unique ctx =
    ctx.ask Queries.MustBeUniqueThread

  let created (current, td) =
    match current with
    | `Lifted current -> BatOption.map_default (ConcDomain.ThreadSet.of_list) (ConcDomain.ThreadSet.top ()) (Thread.created current td)
    | _ -> ConcDomain.ThreadSet.top ()

  let query (ctx: (D.t, _, _, _) ctx) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.CurrentThreadId -> fst ctx.local
    | Queries.CreatedThreads -> created ctx.local
    | Queries.MustBeUniqueThread ->
      begin match fst ctx.local with
        | `Lifted tid -> Thread.is_unique tid
        | _ -> Queries.MustBool.top ()
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
      let tid = fst ctx.local in
      Some tid
    else
      None

  let threadenter ctx lval f args =
    let+ tid = create_tid ctx.local ctx.prev_node f in
    (tid, TD.bot ())

  let threadspawn ctx lval f args fctx =
    let (current, td) = ctx.local in
    (current, Thread.threadspawn td ctx.prev_node f)

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
    Printf.printf "Encountered number of thread IDs (unique): %i (%i)\n" (uc+nc) uc;
    Printf.printf "unique: ";
    List.iter (fun tid -> Printf.printf " %s " (Thread.show tid)) uniques;
    Printf.printf "\nnon-unique: ";
    List.iter (fun tid -> Printf.printf " %s " (Thread.show tid)) non_uniques;
    Printf.printf "\n"

  let finalize () =
    if GobConfig.get_bool "dbg.print_tids" then print_tid_info ();
    !tids
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
