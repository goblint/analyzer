(** Analysis of memory accesses ([access]). *)

module LF = LibraryFunctions
open GoblintCil
open Analyses
open GobConfig


(** Access analyzer without base --- this is the new standard *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "access"

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  module V =
  struct
    include Node
    let is_write_only _ = true
  end
  module G = AccessDomain.EventSet

  let collect_local = ref false
  let emit_single_threaded = ref false

  let init _ =
    collect_local := get_bool "witness.yaml.enabled" && get_bool "witness.invariant.accessed";
    let activated = get_string_list "ana.activated" in
    emit_single_threaded := List.mem (ModifiedSinceSetjmp.Spec.name ()) activated || List.mem (PoisonVariables.Spec.name ()) activated || List.mem (UseAfterFree.Spec.name ()) activated (* TODO: some of these don't have access as dependency *)

  let emit_access (man: (D.t, G.t, C.t, V.t) man) (kind:AccessKind.t) (reach:bool) (e:exp) =
    let reach_or_mpt: _ Queries.t = if reach then ReachableFrom e else MayPointTo e in
    let ad = man.ask reach_or_mpt in
    man.emit (Access {exp=e; ad; kind; reach})

  (* Emits the access of [kind] through [e]. With [stream] a library call
     makes it, and where [e] may point to the object of a standard stream the
     call also reads or writes the bytes of the buffer the object holds
     ({!StandardStreams}), and nothing the buffer's contents point to. That
     access is emitted separately, with {!StandardStreams.buffer_exp} as its
     expression: [`Locked], as for a stdio function that takes the lock on the
     stream, makes {!MutexAnalysis} take it as made under that lock;
     [`Unlocked], as for [putc_unlocked], does not. The access through [e]
     itself is emitted without what is reachable from a stream object's
     value, so it keeps the objects and whatever [e] reaches otherwise. *)
  let do_access ?(stream: [`Locked | `Unlocked] option) (man: (D.t, G.t, C.t, V.t) man) (kind:AccessKind.t) (reach:bool) (e:exp) =
    if M.tracing then M.trace "access" "do_access %a %a %B" d_exp e AccessKind.pretty kind reach;
    let objects = match stream with
      | None -> []
      | Some _ ->
        let mpt = man.ask (MayPointTo e) in
        Queries.AD.fold (fun addr objects ->
            match addr with
            | Queries.AD.Addr.Addr (v, _) when StandardStreams.is_object v && not (List.exists (CilType.Varinfo.equal v) objects) -> v :: objects
            | _ -> objects
          ) mpt []
    in
    match stream with
    | Some locking when objects <> [] ->
      let mpt = man.ask (MayPointTo e) in
      let only_objects = Queries.AD.for_all (function
          | Queries.AD.Addr.Addr (v, _) -> StandardStreams.is_object v
          | NullPtr -> true
          | _ -> false
        ) mpt
      in
      if only_objects || not reach then
        man.emit (Access {exp=e; ad=mpt; kind; reach=false})
      else begin
        let through_buffers = List.fold_left (fun through v ->
            Queries.AD.join through (man.ask (ReachableFrom (Lval (Var v, NoOffset))))
          ) (Queries.AD.empty ()) objects
        in
        let through_buffers = Queries.AD.remove UnknownPtr (Queries.AD.remove NullPtr through_buffers) in
        let ad = Queries.AD.diff (man.ask (ReachableFrom e)) through_buffers in
        man.emit (Access {exp=e; ad; kind; reach})
      end;
      List.iter (fun v ->
          let buffer = StandardStreams.buffer_exp ~locked:(locking = `Locked) v in
          let ad = man.ask (MayPointTo buffer) in
          if not (Queries.AD.is_null ad || Queries.AD.is_empty ad) then
            man.emit (Access {exp=buffer; ad; kind; reach=false})
        ) objects
    | _ ->
      emit_access man kind reach e

  (** Three access levels:
      + [deref=false], [reach=false] - Access [exp] without dereferencing, used for all normal reads and all function call arguments.
      + [deref=true], [reach=false] - Access [exp] by dereferencing once (may-point-to), used for lval writes and shallow special accesses.
      + [deref=true], [reach=true] - Access [exp] by dereferencing transitively (reachable), used for deep special accesses. *)
  let access_one_top ?(force=false) ?(deref=false) ?stream man (kind: AccessKind.t) reach exp =
    if M.tracing then M.traceli "access" "access_one_top %a (kind = %a, reach = %B, deref = %B)" CilType.Exp.pretty exp AccessKind.pretty kind reach deref;
    if force || !collect_local || !emit_single_threaded || ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man) then (
      if deref && Cil.isPointerType (Cilfacade.typeOf exp) then (* avoid dereferencing integers to unknown pointers, which cause many spurious type-based accesses *)
        do_access ?stream man kind reach exp;
      if M.tracing then M.tracei "access" "distribute_access_exp";
      Access.distribute_access_exp (do_access man Read false) exp;
      if M.tracing then M.traceu "access" "distribute_access_exp";
    );
    if M.tracing then M.traceu "access" "access_one_top"


  (** We just lift start state, global and dependency functions: *)
  let startstate v = ()
  let threadenter man ~multiple lval f args = [()]
  let exitstate  v = ()
  let context man fd d = ()


  (** Transfer functions: *)

  let vdecl man v =
    access_one_top man Read false (SizeOf v.vtype);
    man.local

  let assign man lval rval : D.t =
    (* ignore global inits *)
    if !AnalysisState.global_initialization then man.local else begin
      access_one_top ~deref:true man Write false (AddrOf lval);
      access_one_top man Read false rval;
      man.local
    end

  let branch man exp tv : D.t =
    access_one_top man Read false exp;
    man.local

  let return man exp fundec : D.t =
    Option.iter (access_one_top man Read false) exp;
    man.local

  let body man f : D.t =
    man.local

  let special man lv f arglist : D.t =
    let desc = LF.find f in
    match desc.special arglist with
    (* TODO: remove Lock/Unlock cases when all those libraryfunctions use librarydescs and don't read mutex contents *)
    | Lock _
    | Unlock _ ->
      man.local
    | _ ->
      let stream = if List.mem LibraryDesc.StreamUnlocked desc.attrs then `Unlocked else `Locked in
      LibraryDesc.Accesses.iter desc.accs (fun {kind; deep = reach} exp ->
          access_one_top ~deref:true ~stream man kind reach exp (* access dereferenced using special accesses *)
        ) arglist;
      (* A standard stream used without being an argument, as [printf] uses
         [stdout] and [fflush(NULL)] every stream, is read and written deeply.
         The variable [stdout] is read as an access of its own only where the
         program may assign it. *)
      let first_may_be_null () = match arglist with
        | arg :: _ ->
          let ad = man.ask (Queries.MayPointTo arg) in
          Queries.AD.is_top ad || Queries.AD.exists (function NullPtr -> true | _ -> false) ad
        | [] -> false
      in
      List.iter (fun s ->
          let e = StandardStreams.exp s in
          if StandardStreams.assigned s then (
            access_one_top ~deref:true ~stream man Read true e;
            access_one_top ~deref:true ~stream man Write true e
          )
          else if !collect_local || !emit_single_threaded || ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man) then (
            do_access ~stream man Read true e;
            do_access ~stream man Write true e
          )
        ) (LF.implicit_streams desc ~first_may_be_null);
      Option.iter (fun x -> access_one_top ~deref:true man Write false (AddrOf x)) lv;
      List.iter (access_one_top man Read false) arglist; (* always read all argument expressions without dereferencing *)
      man.local

  let enter man lv f args : (D.t * D.t) list =
    [(man.local,man.local)]

  let combine_env man lval fexp f args fc au f_ask =
    (* These should be in enter, but enter cannot emit events, nor has fexp argument *)
    access_one_top man Read false fexp;
    List.iter (access_one_top man Read false) args;
    au

  let combine_assign man lv fexp f args fc al f_ask =
    Option.iter (fun lval -> access_one_top ~deref:true man Write false (AddrOf lval)) lv;
    man.local


  let threadspawn man  ~multiple lval f args fman =
    (* must explicitly access thread ID lval because special to pthread_create doesn't if singlethreaded before *)
    Option.iter (fun lval ->
        access_one_top ~force:true ~deref:true man Write false (AddrOf lval) (* must force because otherwise doesn't if singlethreaded before *)
      ) lval;
    man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayAccessed ->
      (man.global man.node: G.t)
    | _ -> Queries.Result.top q

  let event man e oman =
    match e with
    | Events.Access {ad; kind; _} when !collect_local && !AnalysisState.postsolving ->
      let events = Queries.AD.fold (fun addr es ->
          match addr with
          | Queries.AD.Addr.Addr (var, offs) ->
            let coffs = ValueDomain.Offs.to_cil offs in
            let access: AccessDomain.Event.t = {var_opt = (Some var); offs_opt = (Some coffs); kind} in
            G.add access es
          | UnknownPtr ->
            let access: AccessDomain.Event.t = {var_opt = None; offs_opt = None; kind} in
            G.add access es
          | _ -> es
        ) ad (G.empty ())
      in
      man.sideg man.node events
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
