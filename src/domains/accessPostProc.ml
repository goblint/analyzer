open Access
open Batteries
open GoblintCil
open Pretty
open GobConfig

module M = Messages
module RM = RepositionMessages

let g_accs : AS.t list ref = ref []

(* Check if two accesses may race and if yes with which confidence *)
let may_race (conf,(kind: AccessKind.t),loc,e,a) (conf2,(kind2: AccessKind.t),loc2,e2,a2) =
  if kind = Read && kind2 = Read then
    false (* two read/read accesses do not race *)
  else if not (get_bool "ana.race.free") && (kind = Free || kind2 = Free) then
    false
  else if not (MCPAccess.A.may_race a a2) then
    false (* analysis-specific information excludes race *)
  else
    true

let group_may_race accs =
  (* BFS to traverse one component with may_race edges *)
  let rec bfs' accs visited todo =
    let accs' = AS.diff accs todo in
    let todo' = AS.fold (fun acc todo' ->
        AS.fold (fun acc' todo' ->
            if may_race acc acc' then
              AS.add acc' todo'
            else
              todo'
          ) accs' todo'
      ) todo (AS.empty ())
    in
    let visited' = AS.union visited todo in
    if AS.is_empty todo' then
      (accs', visited')
    else
      (bfs' [@tailcall]) accs' visited' todo'
  in
  let bfs accs acc = bfs' accs (AS.empty ()) (AS.singleton acc) in
  (* repeat BFS to find all components *)
  let rec components comps accs =
    if AS.is_empty accs then
      comps
    else (
      let acc = AS.choose accs in
      let (accs', comp) = bfs accs acc in
      let comps' = comp :: comps in
      components comps' accs'
    )
  in
  components [] accs

let race_conf accs =
  assert (not (AS.is_empty accs)); (* group_may_race should only construct non-empty components *)
  if AS.cardinal accs = 1 then ( (* singleton component *)
    let acc = AS.choose accs in
    if may_race acc acc then (* self-race *)
      Some (A.conf acc)
    else
      None
  )
  else
    Some (AS.max_conf accs)

let race_severity conf : Messages.Severity.t  =
  let race_threshold = get_int "warn.race-threshold" in
  if conf >= race_threshold then
    Warning
  else
    Info

let is_all_safe = ref true

(* Commenting your code is for the WEAK! *)
let incr_summary safe vulnerable unsafe (lv, ty) grouped_accs =
  (* ignore(printf "Checking safety of %a:\n" d_memo (ty,lv)); *)
  let safety =
    grouped_accs
    |> List.filter_map race_conf
    |> (function
        | [] -> None
        | confs -> Some (List.max confs)
      )
  in
  match safety with
  | None -> incr safe
  | Some n when n >= 100 -> is_all_safe := false; incr unsafe
  | Some n -> is_all_safe := false; incr vulnerable

let print_accesses (lv, ty) grouped_accs =
  let allglobs = get_bool "allglobs" in
  let debug = get_bool "dbg.debug" in
  let h (conf,kind,node,e,a) =
    let d_msg () = dprintf "%a with %a (conf. %d)" AccessKind.pretty kind MCPAccess.A.pretty a conf in
    let doc =
      if debug then
        dprintf "%t  (exp: %a)" d_msg d_exp e
      else
        d_msg ()
    in
    (doc, Some (Messages.Location.Node node))
  in
  let msgs race_accs =
    AS.elements race_accs
    |> List.map h
  in
  let loc (_,_,node,_,_) = M.Location.Node node in
  let filter_no_race a accs = AS.filter (fun a1 -> may_race a a1) accs in
  grouped_accs
  |> List.fold_left (fun safe_accs accs ->
      match race_conf accs with
      | None -> AS.union safe_accs accs (* group all safe accs together for allglobs *)
      | Some conf ->
        if get_bool "ana.warn-postprocess.enabled" then (
          accs |> AS.iter (fun a -> RM.msg_group (race_severity (A.conf a)) (Acc (a, filter_no_race a accs, accs)) ~loc:(loc a) ~category:Race "Memory location %a (race with conf. %d)" d_memo (ty,lv) conf [h a]);
          safe_accs) 
        else
          let severity = race_severity conf in
          M.msg_group severity ~category:Race "Memory location %a (race with conf. %d)" d_memo (ty,lv) conf (msgs accs);
          safe_accs
    ) (AS.empty ())
  |> (fun safe_accs ->
      if allglobs && not (AS.is_empty safe_accs) then
        if get_bool "ana.warn-postprocess.enabled"
        then safe_accs |> AS.iter (fun a -> RM.msg_group (race_severity (A.conf a)) (Acc (a, filter_no_race a safe_accs, safe_accs)) ~loc:(loc a) ~category:Race "" [h a])
        else M.msg_group Success ~category:Race "Memory location %a (safe)" d_memo (ty,lv) (msgs safe_accs)
    )

let warn_global safe vulnerable unsafe g accs =
  let grouped_accs = group_may_race accs in (* do expensive component finding only once *)
  g_accs := grouped_accs;
  incr_summary safe vulnerable unsafe g grouped_accs;
  print_accesses g grouped_accs