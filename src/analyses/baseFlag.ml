(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Flag = BaseDomain.Flag
  module D = Flag
  module C = Flag
  module G = Lattice.Unit

  let name () = "baseflag"

  let startstate v = Flag.bot ()
  let otherstate v = Flag.start_multi v
  let exitstate  v = Flag.start_main v

  let morphstate v _ = Flag.start_single v

  let create_tid v =
    let loc = !Tracing.current_loc in
    Flag.spawn_thread loc v
  let threadstate v = create_tid v

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      Flag.make_main ctx.local
    | "StartupHook" ->
      Flag.get_multi ()
    | _ ->
      ctx.local

  let assign ctx (lval:lval) (rval:exp) : D.t  = ctx.local

  let enter ctx lval f args =
    [ctx.local,ctx.local]

  let combine ctx lval fexp f args fc st2 = st2

  open Queries
  let collect_funargs ctx (exps: exp list) =
    let do_exp e =
      match ctx.ask (ReachableFrom e) with
      | `LvalSet ls when not (LS.is_top ls) ->
        List.map fst (LS.elements ls)
      | _-> []
    in
    List.concat (List.map do_exp exps)

  let forkfun (ctx:(D.t, G.t, C.t) Analyses.ctx) (lv: lval option) (f: varinfo) (args: exp list) : (varinfo * D.t) list =
    let create_thread arg v =
      try
        let nfl = create_tid v in
        let nst = nfl in
        Some (v, nst)
      with Not_found ->
        if LF.use_special f.vname then None (* we handle this function *)
        else if isFunctionType v.vtype then (
          M.warn_each ("Creating a thread from unknown function " ^ v.vname);
          Some (v, create_tid v)
        ) else (
          M.warn_each ("Not creating a thread from " ^ v.vname ^ " because its type is " ^ sprint d_type v.vtype);
          None
        )
    in
    match LF.classify f.vname args with
    (* handling thread creations *)
    | `Unknown "LAP_Se_SetPartitionMode" when List.length args = 2 -> begin
        let mode = List.hd @@ List.map (fun x -> stripCasts (constFold false x)) args in
        match ctx.ask (Queries.EvalInt mode) with
        | `Int i when i=3L -> begin
          let open Queries in
          match ctx.ask (MayPointTo (Lval (var (Base.Main.tasks_var ())))) with
          | `LvalSet ls ->
            let varinfos = List.map fst (LS.elements ls) in
            List.filter_map (create_thread None) varinfos
          | _ -> failwith "baseflag setpartitionmode"
          end
        | _ -> []
      end
    | `ThreadCreate (start,ptc_arg) -> begin
        (* Collect the threads. *)
        let open Queries in
        match ctx.ask (MayPointTo start) with
        | `LvalSet ls ->
          let varinfos = List.map fst (LS.elements ls) in
          List.filter_map (create_thread (Some ptc_arg)) varinfos
        | _ -> failwith "baseflag threadcreate"
      end
    | `Unknown _ -> begin
        let args =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> fnc `Write  args (* why do we only spawn arguments that are written?? *)
          | None -> args
        in
        let flist = collect_funargs ctx args in
        List.filter_map (create_thread None) flist
      end
    | _ ->  []

  let special ctx lval f args =
    let forks = forkfun ctx lval f args in
    if M.tracing then M.tracel "spawn" "Base.special %s: spawning functions %a\n" f.vname (d_list "," d_varinfo) (List.map fst forks);
    List.iter (uncurry ctx.spawn) forks;
    match LF.classify f.vname args with
    | `Unknown "LAP_Se_SetPartitionMode" -> begin
        match ctx.ask (Queries.EvalInt (List.hd args)) with
        | `Int i when i=1L || i=2L -> ctx.local
        | `Bot -> ctx.local
        | _ -> Flag.make_main ctx.local
      end
    | `ThreadCreate (f,x) -> Flag.make_main ctx.local
    | _ -> begin
      match LF.get_invalidate_action f.vname with
      | Some _ -> ctx.local
      | None -> (
          (* This rest here is just to see if something got spawned. *)
          let flist = collect_funargs ctx args in
          let f addr acc =
            try
              let var = addr in
              let _ = Cilfacade.getdec var in true
            with _ -> acc
          in
          if List.fold_right f flist false
          && not (GobConfig.get_bool "exp.single-threaded")
          && GobConfig.get_bool "exp.unknown_funs_spawn" then
            Flag.make_main ctx.local
          else
            ctx.local
        )
    end


  let query ctx x =
    match x with
    | Queries.SingleThreaded -> `Bool (Queries.BD.of_bool (not (Flag.is_multi ctx.local)))
    | _ -> `Top


  let is_unique ctx fl =
    not (BaseDomain.Flag.is_bad fl) ||
    match ctx.ask Queries.IsNotUnique with
    | `Bool false -> true
    | _ -> false

  (* remove this function and everything related to exp.ignored_threads *)
  let is_special_ignorable_thread = function
    | (_, `Lifted f) ->
      let fs = GobConfig.get_list "exp.ignored_threads" |> List.map Json.string in
      List.mem f.vname fs
    | _ -> false

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    let fl = ctx.local in
    if BaseDomain.Flag.is_multi fl && not (is_special_ignorable_thread fl) then begin
      if is_unique ctx fl then
        let tid = BaseDomain.Flag.short 20 fl in
        (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
      else
        (Access.LSSSet.singleton es, es)
    end else
      Access.LSSSet.empty (), es

end

let _ =
  MCP.register_analysis (module Spec : Spec)
