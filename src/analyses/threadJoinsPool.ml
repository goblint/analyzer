(** Join-coverage analysis for thread handles stored in arrays ([threadJoinsPool]).

    A concrete, joinable thread created by the current thread gives rise to one
    outstanding obligation.  For supported local [pthread_t] objects, that
    obligation is kept in the exact scalar or array slot passed to
    [pthread_create].  Joining that same slot consumes the obligation.

    Array slots are represented by a length-aware partitioned array domain.
    This lets a creation loop establish that every element contains an
    obligation and a subsequent join loop establish that every element has
    consumed it.  Abstract thread IDs may be non-unique: distinct dynamic
    instances remain distinguished by their canonical array slots.

    Whenever canonical storage is overwritten, escapes, is passed to an
    unsupported operation, or cannot be represented precisely enough, its
    obligations are moved to the permanent [lost] set.  A thread ID is only
    reported as must-joined if no instance of it remains in a slot or in
    [lost].  This is the coverage condition missing from the proof of concept
    in https://github.com/goblint/analyzer/pull/1180.  Copies between distinct
    [pthread_t] objects are deliberately treated as lost: the supported
    precise idiom uses a fixed-size local array, a constant or plain local
    scalar index, and joins the same direct slot that received the handle. *)

open GoblintCil
open Analyses

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module MustTIDs = ConcDomain.FiniteMustThreadSet
module AD = Queries.AD

module Live =
struct
  include TIDs

  let name () = "outstanding thread joins"
  let invalidate_abstract_value _ = top ()
  let smart_join _ _ = join
  let smart_widen _ _ = widen
  let smart_leq _ _ = leq
end

module PoolArray = ArrayDomain.PartitionedWithLength (Live) (ValueDomain.IndexDomain)
module Arrays = MapDomain.PatriciaMapBot (Basetype.Variables) (PoolArray)
module Scalars = MapDomain.PatriciaMapBot (Basetype.Variables) (Live)
module UnsafeVars = EscapeDomain.EscapedVars

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadJoinsPool"

  module Meta = Lattice.Prod (Live) (UnsafeVars)
  (** Partitioned arrays * scalar handles * (lost obligations * unsafe roots). *)
  module D = Lattice.Prod3 (Arrays) (Scalars) (Meta)
  include Analyses.ValueContexts (D)
  module G = Lattice.Lift (BoolDomain.MustBool)
  module V =
  struct
    include TID
    include StdV
  end
  module P = IdentityP (D)

  let empty_state () = (Arrays.empty (), Scalars.empty (), (Live.empty (), UnsafeVars.empty ()))

  let arrays (a, _, _) = a
  let scalars (_, s, _) = s
  let lost (_, _, (l, _)) = l
  let unsafe (_, _, (_, u)) = u

  let with_arrays a (_, s, m) = (a, s, m)
  let with_scalars s (a, _, m) = (a, s, m)
  let with_lost l (a, s, (_, u)) = (a, s, (l, u))
  let with_unsafe u (a, s, (l, _)) = (a, s, (l, u))

  let add_lost tids st =
    with_lost (Live.join (lost st) tids) st

  let lose_all st =
    let live_arrays =
      Arrays.fold (fun _ a acc -> PoolArray.fold_left Live.join acc a) (arrays st) (Live.empty ())
    in
    let live_scalars =
      Scalars.fold (fun _ tids acc -> Live.join acc tids) (scalars st) (Live.empty ())
    in
    add_lost (Live.join live_arrays live_scalars) st

  let rec is_thread_type = function
    | TNamed ({tname = "pthread_t"; _}, _) -> true
    | TNamed ({ttype; _}, _) -> is_thread_type ttype
    | _ -> false

  let rec array_info = function
    | TNamed ({tname = "pthread_t"; _}, _) -> None
    | TNamed ({ttype; _}, _) -> array_info ttype
    | TArray (element, Some length, _)
      when is_thread_type element && Basetype.CilExp.get_vars length = [] -> Some length
    | _ -> None

  let supported_index index =
    match Cil.stripCasts index with
    | Lval (Var v, NoOffset) ->
      not v.vglob && Cil.isIntegralType v.vtype
    | index -> Basetype.CilExp.get_vars index = []

  let normalize_lval lval =
    match lval with
    | Mem pointer, NoOffset ->
      begin match Cil.stripCasts pointer with
        | AddrOf lval -> lval
        | _ -> lval
      end
    | _ -> lval

  let direct_storage lval =
    match normalize_lval lval with
    | (Var v, NoOffset) when is_thread_type v.vtype -> `Scalar v
    | (Var v, Index (index, NoOffset))
      when Option.is_some (array_info v.vtype) && supported_index index -> `Array (v, index)
    | (Var v, _) -> `OtherRoot v
    | _ -> `Indirect

  let index_value_of_eval = function
    | `Lifted i -> ValueDomain.ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) i
    | `Bot -> ValueDomain.IndexDomain.bot ()
    | `Top -> ValueDomain.IndexDomain.top ()

  let index_value (ask: Queries.ask) index =
    index_value_of_eval (ask.f (Queries.EvalInt index))

  let make_array (ask: Queries.ask) v =
    match array_info v.vtype with
    | Some length -> PoolArray.make (index_value ask length) (Live.empty ())
    | None -> PoolArray.bot ()

  let find_array (ask: Queries.ask) v st =
    match Arrays.find_opt v (arrays st) with
    | Some a -> a
    | None -> make_array ask v

  let array_index (ask: Queries.ask) index = Some index, index_value ask index

  let add_unsafe_var v st =
    with_unsafe (UnsafeVars.add v (unsafe st)) st

  let var_is_unsafe (ask: Queries.ask) st v =
    UnsafeVars.is_top (unsafe st)
    || UnsafeVars.mem v (unsafe st)
    || v.vglob
    || ThreadEscape.has_escaped ask v

  let is_thread_storage_root v =
    is_thread_type v.vtype || Option.is_some (array_info v.vtype)

  let poison_var v st =
    let st =
      match Arrays.find_opt v (arrays st) with
      | Some a ->
        let tids = PoolArray.fold_left Live.join (Live.empty ()) a in
        add_lost tids st
      | None -> st
    in
    let st =
      match Scalars.find_opt v (scalars st) with
      | Some tids -> add_lost tids st
      | None -> st
    in
    st
    |> with_arrays (Arrays.remove v (arrays st))
    |> with_scalars (Scalars.remove v (scalars st))

  let poison_all st =
    let st = lose_all st in
    (Arrays.empty (), Scalars.empty (), (lost st, UnsafeVars.top ()))

  let overwrite_direct (ask: Queries.ask) lval st =
    let vdask = Queries.to_value_domain_ask ask in
    match direct_storage lval with
    | `Array (v, index) ->
      begin match Arrays.find_opt v (arrays st) with
        | Some a ->
          let ai = array_index ask index in
          let old = PoolArray.get ~checkBounds:false vdask a ai in
          let st = add_lost old st in
          with_arrays (Arrays.add v (PoolArray.set vdask a ai (Live.empty ())) (arrays st)) st
        | None -> st
      end
    | `Scalar v ->
      begin match Scalars.find_opt v (scalars st) with
        | Some old ->
          let st = add_lost old st in
          with_scalars (Scalars.remove v (scalars st)) st
        | None -> st
      end
    | `OtherRoot v when Arrays.mem v (arrays st) || Scalars.mem v (scalars st) ->
      poison_var v st
    | `OtherRoot _
    | `Indirect -> st

  let supported_storage = function
    | (`Scalar _ | `Array _) as storage -> Some storage
    | `OtherRoot _
    | `Indirect -> None

  let storage_of_exp exp =
    match Cil.stripCasts exp with
    | Lval lval -> supported_storage (direct_storage lval)
    | _ -> None

  let storage_root = function
    | `Scalar v
    | `Array (v, _) -> v

  let owner_at (ask: Queries.ask) st = function
    | `Scalar v -> Scalars.find v (scalars st)
    | `Array (v, index) ->
      begin match Arrays.find_opt v (arrays st) with
        | None -> Live.empty ()
        | Some a ->
          PoolArray.get ~checkBounds:false (Queries.to_value_domain_ask ask) a (array_index ask index)
      end

  let clear_owner (ask: Queries.ask) storage st =
    match storage with
    | `Scalar v -> with_scalars (Scalars.remove v (scalars st)) st
    | `Array (v, index) ->
      begin match Arrays.find_opt v (arrays st) with
        | None -> st
        | Some a ->
          let vdask = Queries.to_value_domain_ask ask in
          let a = PoolArray.set vdask a (array_index ask index) (Live.empty ()) in
          with_arrays (Arrays.add v a (arrays st)) st
      end

  type storage_relation = Same | Distinct | Ambiguous

  let storage_relation (ask: Queries.ask) left right =
    match left, right with
    | `Scalar x, `Scalar y ->
      if CilType.Varinfo.equal x y then Same else Distinct
    | `Scalar _, `Array _
    | `Array _, `Scalar _ -> Distinct
    | `Array (x, _), `Array (y, _) when not (CilType.Varinfo.equal x y) -> Distinct
    | `Array (_, xi), `Array (_, yi) ->
      if Queries.must_be_equal ask xi yi then
        Same
      else if not (Queries.may_be_equal ask xi yi) then
        Distinct
      else
        Ambiguous

  let transfer_copy (ask: Queries.ask) destination source st =
    match storage_relation ask destination source with
    | Same -> st
    | Ambiguous ->
      (* A weakly identified copy may alias either owner. *)
      add_unsafe_var (storage_root source) (poison_var (storage_root source) st)
    | Distinct ->
      let carried = owner_at ask st source in
      let overwritten = owner_at ask st destination in
      (* C copies, rather than moves, a [pthread_t].  Without tracking every
         alias, either copy could be used to join first and make a later join
         through the other fail.  Permanently losing the obligation is the
         sound conservative abstraction for copies. *)
      let st = add_lost (Live.join carried overwritten) st in
      let st = clear_owner ask source st in
      clear_owner ask destination st

  let poison_exp_owners exp st =
    List.fold_left (fun st v ->
        if Arrays.mem v (arrays st) || Scalars.mem v (scalars st) then
          poison_var v st
        else
          st
      ) st (Basetype.CilExp.get_vars exp)

  let poison_exp_list_owners exps st =
    List.fold_left (fun st exp -> poison_exp_owners exp st) st exps

  let movement_for_assignment (ask: Queries.ask) variable rhs pivot =
    try
      let typ = Cilfacade.typeOf pivot in
      let ik = Cilfacade.get_ikind typ in
      let moved = Basetype.CilExp.replace variable rhs pivot in
      let one = Cil.kinteger ik 1 in
      if Queries.must_be_equal ask moved (BinOp (PlusA, pivot, one, typ)) then
        Some 1
      else if Queries.must_be_equal ask moved (BinOp (MinusA, pivot, one, typ)) then
        Some (-1)
      else
        None
    with Cilfacade.TypeOfError _ ->
      None

  let move_partition_pivots (ask: Queries.ask) variable rhs st =
    let vdask = Queries.to_value_domain_ask ask in
    let moved =
      Arrays.map (fun a ->
          PoolArray.move_if_affected vdask a variable (movement_for_assignment ask variable rhs)
        ) (arrays st)
    in
    (* [move_if_affected] checks array bounds before shifting the pivot.
       Check once more in the post-assignment integer state so that the final
       i++ of a loop collapses the now-complete partition. *)
    let post_vdask =
      { vdask with
        eval_int = (fun e -> vdask.eval_int (Basetype.CilExp.replace variable rhs e))
      }
    in
    let moved =
      Arrays.map (fun a ->
          PoolArray.move_if_affected post_vdask a variable (fun _ -> Some 0)
        ) moved
    in
    (* A completed creation loop has become [Joint live].  At a boundary reset
       of a subsequent join loop, repartition that value around the new
       induction variable.  Entry and back-edge states then use the same
       pivot, so the ordinary lattice join preserves the processed-prefix (or
       suffix) invariant. *)
    let pivot = Lval (Var variable, NoOffset) in
    let pivot_value = Queries.ID.to_int (post_vdask.eval_int pivot) in
    let moved =
      Arrays.map (fun a ->
          match PoolArray.length a with
          | Some length ->
            let at_boundary =
              match pivot_value, ValueDomain.IndexDomain.to_int length with
              | Some i, _ when Z.equal i Z.zero -> true
              | Some i, Some length when Z.equal i (Z.pred length) -> true
              | _ -> false
            in
            if at_boundary then
              let all = PoolArray.fold_left Live.join (Live.empty ()) a in
              let joint = PoolArray.make length all in
              let index = index_value_of_eval (post_vdask.eval_int pivot) in
              PoolArray.set post_vdask joint (Some pivot, index) all
            else
              a
          | None -> a
        ) moved
    in
    with_arrays moved st

  let vars_of_addresses addresses =
    if AD.is_top addresses then
      None
    else
      Some (AD.fold (fun address vars ->
          match address with
          | AD.Addr.Addr (v, _) -> UnsafeVars.add v vars
          | _ -> vars
        ) addresses (UnsafeVars.empty ()))

  let invalidate_partition_variables (ask: Queries.ask) variables st =
    let vdask = Queries.to_value_domain_ask ask in
    let invalidate a =
      UnsafeVars.fold (fun variable a ->
          PoolArray.move_if_affected vdask a variable (fun _ -> None)
        ) variables a
    in
    with_arrays (Arrays.map invalidate (arrays st)) st

  let poison_indirect_write (ask: Queries.ask) lval st =
    match vars_of_addresses (ask.f (Queries.MayPointTo (AddrOf lval))) with
    | None -> poison_all st
    | Some vars ->
      let st = UnsafeVars.fold (fun v st ->
          if Arrays.mem v (arrays st) || Scalars.mem v (scalars st) || is_thread_storage_root v then
            add_unsafe_var v (poison_var v st)
          else
            st
        ) vars st
      in
      invalidate_partition_variables ask vars st

  let invalidate_lval (ask: Queries.ask) lval st =
    let st =
      match direct_storage lval with
      | `Indirect -> poison_indirect_write ask lval st
      | _ -> overwrite_direct ask lval st
    in
    match normalize_lval lval with
    | Var variable, NoOffset ->
      let variables = UnsafeVars.singleton variable in
      invalidate_partition_variables ask variables st
    | _ -> st

  let assign man lval rhs =
    let ask = Analyses.ask_of_man man in
    let st =
      match supported_storage (direct_storage lval), storage_of_exp rhs with
      | Some destination, Some source -> transfer_copy ask destination source man.local
      | _, _ ->
        let st = poison_exp_owners rhs man.local in
        begin match direct_storage lval with
          | `Indirect -> poison_indirect_write ask lval st
          | _ -> overwrite_direct ask lval st
        end
    in
    match normalize_lval lval with
    | Var variable, NoOffset -> move_partition_pivots ask variable rhs st
    | _ -> st

  let store_spawned (ask: Queries.ask) ~multiple lval tid st =
    let unsupported () = add_lost (Live.singleton tid) st in
    if multiple then
      unsupported ()
    else
      match lval with
      | None -> unsupported ()
      | Some lval ->
        let vdask = Queries.to_value_domain_ask ask in
        begin match direct_storage lval with
          | `Array (v, index) when not (var_is_unsafe ask st v) ->
            let a = find_array ask v st in
            let ai = array_index ask index in
            let old = PoolArray.get ~checkBounds:false vdask a ai in
            let st = add_lost old st in
            let a = PoolArray.set vdask a ai (Live.singleton tid) in
            with_arrays (Arrays.add v a (arrays st)) st
          | `Scalar v when not (var_is_unsafe ask st v) ->
            let old = Scalars.find v (scalars st) in
            let st = add_lost old st in
            with_scalars (Scalars.add v (Live.singleton tid) (scalars st)) st
          | `Array (v, _)
          | `Scalar v -> add_unsafe_var v (unsupported ())
          | `OtherRoot _
          | `Indirect -> unsupported ()
        end

  let threadspawn man ~multiple lval f args fman =
    match fman.ask Queries.CurrentThreadId with
    | `Lifted tid -> store_spawned (Analyses.ask_of_man man) ~multiple lval tid man.local
    | `Bot
    | `Top -> add_lost (Live.top ()) man.local

  let compatible live evaluated =
    not (Live.is_top evaluated)
    && not (Live.is_empty evaluated)
    && Live.leq live evaluated

  let join_direct (ask: Queries.ask) id st =
    let vdask = Queries.to_value_domain_ask ask in
    let evaluated = ask.f (Queries.EvalThread id) in
    match Cil.stripCasts id with
    | Lval lval ->
      begin match direct_storage lval with
        | `Array (v, index) ->
          begin match Arrays.find_opt v (arrays st) with
            | Some a ->
              let ai = array_index ask index in
              let live = PoolArray.get ~checkBounds:false vdask a ai in
              if Live.is_empty live then
                st
              else if compatible live evaluated then
                with_arrays (Arrays.add v (PoolArray.set vdask a ai (Live.empty ())) (arrays st)) st
              else
                add_lost live st
            | None -> st
          end
        | `Scalar v ->
          begin match Scalars.find_opt v (scalars st) with
            | Some live when compatible live evaluated ->
              with_scalars (Scalars.remove v (scalars st)) st
            | Some live -> add_lost live st
            | None -> st
          end
        | `OtherRoot _
        | `Indirect -> poison_exp_owners id st
      end
    | _ -> poison_exp_owners id st

  let pointer_argument_roots (ask: Queries.ask) arg =
    if Cil.isPointerType (Cilfacade.typeOf arg) then
      vars_of_addresses (ask.f (Queries.ReachableFrom arg))
    else
      Some (UnsafeVars.empty ())

  let poison_pointer_arguments (ask: Queries.ask) args st =
    List.fold_left (fun st arg ->
        match pointer_argument_roots ask arg with
        | None -> poison_all st
        | Some vars ->
          let st = UnsafeVars.fold (fun v st ->
              if Arrays.mem v (arrays st) || Scalars.mem v (scalars st) || is_thread_storage_root v then
                add_unsafe_var v (poison_var v st)
              else
                st
            ) vars st
          in
          invalidate_partition_variables ask vars st
      ) st args

  let created_with_nondefault_attributes (ask: Queries.ask) args =
    match args with
    | _thread :: attr :: _ when AD.is_null (ask.f (Queries.MayPointTo attr)) -> false
    | _ -> true

  let normalize_array_bounds (ask: Queries.ask) a =
    let vdask = Queries.to_value_domain_ask ask in
    List.fold_left (fun a variable ->
        PoolArray.move_if_affected vdask a variable (fun _ -> Some 0)
      ) a (PoolArray.get_vars_in_e a)

  let all_outstanding (ask: Queries.ask) st =
    let from_arrays =
      Arrays.fold (fun _ a acc ->
          PoolArray.fold_left Live.join acc (normalize_array_bounds ask a)
        ) (arrays st) (Live.empty ())
    in
    let from_scalars =
      Scalars.fold (fun _ tids acc -> Live.join acc tids) (scalars st) (Live.empty ())
    in
    Live.join (lost st) (Live.join from_arrays from_scalars)

  let tid_exited_cleanly man tid =
    match man.global tid with
    | `Lifted true -> true
    | `Bot
    | `Lifted false
    | `Top -> false

  let tid_is_joined_cleanly man outstanding = function
    | ThreadIdDomain.UnknownThread -> false
    | (ThreadIdDomain.Thread _ as tid) ->
      not (Live.is_top outstanding)
      && not (Live.mem tid outstanding)
      && tid_exited_cleanly man tid

  let all_created_joined_cleanly man =
    let created = man.ask Queries.CreatedThreads in
    let outstanding = all_outstanding (Analyses.ask_of_man man) man.local in
    not (Live.is_top created)
    && Live.is_empty outstanding
    && Live.for_all (tid_is_joined_cleanly man outstanding) created

  let record_thread_exit man =
    match man.ask Queries.CurrentThreadId with
    | `Lifted tid -> man.sideg tid (`Lifted (all_created_joined_cleanly man))
    | `Bot
    | `Top -> ()

  let lose_detached_owner (ask: Queries.ask) id st =
    let st = add_lost (ask.f (Queries.EvalThread id)) st in
    match Cil.stripCasts id with
    | Lval lval ->
      begin match supported_storage (direct_storage lval) with
        | Some owner ->
          let st = add_lost (owner_at ask st owner) st in
          clear_owner ask owner st
        | None -> poison_exp_owners id st
      end
    | _ -> poison_exp_owners id st

  let special man lval f args =
    let ask = Analyses.ask_of_man man in
    let desc = LibraryFunctions.find f in
    let invalidate_result st = match lval with Some lval -> invalidate_lval ask lval st | None -> st in
    match desc.special args, f.vname with
    | ThreadJoin {thread = id; ret_var}, _ ->
      let st = join_direct ask id man.local in
      let st = if AD.is_null (ask.f (Queries.MayPointTo ret_var)) then st else poison_pointer_arguments ask [ret_var] st in
      invalidate_result st
    | ThreadCreate {arg; _}, _ ->
      let st = if created_with_nondefault_attributes ask args then add_lost (Live.top ()) man.local else man.local in
      let st = poison_pointer_arguments ask [arg] st in
      invalidate_result st
    | ThreadExit _, _ ->
      record_thread_exit man;
      man.local
    | _, "pthread_detach" ->
      let st = begin match args with
        | id :: _ -> lose_detached_owner ask id man.local
        | [] -> add_lost (Live.top ()) man.local
      end in
      invalidate_result st
    | _, _ ->
      let st = invalidate_result man.local in
      let st = poison_exp_list_owners args st in
      poison_pointer_arguments ask args st

  let drop_var v st =
    poison_var v st

  let return man exp f =
    if ThreadReturn.is_current (Analyses.ask_of_man man) then record_thread_exit man;
    List.fold_left (fun st v -> drop_var v st) man.local (f.sformals @ f.slocals)

  let sync man _ =
    let ask = Analyses.ask_of_man man in
    with_arrays (Arrays.map (normalize_array_bounds ask) (arrays man.local)) man.local

  let enter man lval f args =
    let ask = Analyses.ask_of_man man in
    let st = poison_exp_list_owners args man.local in
    let st = poison_pointer_arguments ask args st in
    [st, st]

  let asm man =
    poison_all man.local

  let combine_assign man lval fexp f args fc au f_ask =
    match lval with
    | Some lval -> invalidate_lval (Analyses.ask_of_man man) lval man.local
    | None -> man.local

  let event man event oman =
    match event with
    | Events.Escape escaped ->
      if UnsafeVars.is_top escaped then
        poison_all man.local
      else
        let st = invalidate_partition_variables (Analyses.ask_of_man man) escaped man.local in
        UnsafeVars.fold (fun v st -> add_unsafe_var v (poison_var v st)) escaped st
    | Events.Longjmped _ -> poison_all man.local
    | _ -> man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustJoinedThreads ->
      let created = man.ask Queries.CreatedThreads in
      if Live.is_top created then
        MustTIDs.empty ()
      else
        let outstanding = all_outstanding (Analyses.ask_of_man man) man.local in
        Live.fold (fun tid joined ->
            match tid with
            | ThreadIdDomain.Thread ft when tid_is_joined_cleanly man outstanding tid -> MustTIDs.add ft joined
            | ThreadIdDomain.Thread _
            | ThreadIdDomain.UnknownThread -> joined
          ) created (MustTIDs.empty ())
    | Queries.MustBeSingleThreaded {since_start = false} ->
      begin match man.ask Queries.CurrentThreadId with
        | `Lifted tid when TID.is_main tid -> all_created_joined_cleanly man
        | `Lifted _
        | `Bot
        | `Top -> false
      end
    | _ -> Queries.Result.top q

  let startstate _ = empty_state ()
  let exitstate _ = empty_state ()
  let threadenter man ~multiple lval f args = [empty_state ()]
end

let _ =
  MCP.register_analysis ~dep:["base"; "threadid"; "threadreturn"; "escape"] (module Spec : MCPSpec)
