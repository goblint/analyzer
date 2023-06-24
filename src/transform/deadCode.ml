(** Dead code elimination transformation ([remove_dead_code]). *)

open Batteries
open GoblintCil
open GobConfig
open MyCFG

(* create a new empty block; fields are mutable hence the function *)
let empty_block () = { battrs = [] ; bstmts = [] }

(** Filter statements out of a block (recursively). CFG fields (prev/next,
    Loop continue/break) are no longer valid after calling. Returns true if
    anything is left in block, false if the block is now empty. Invariants:
    - f (goto label) ==> f (labelled stmt), i.e. if a goto statement is not
      filtered out, the target may not be filtered out either.
    - block may not contain switch statements. *)
let filter_map_block ?(unchecked_condition = Fun.const (GoblintCil.integer 1)) f block =
  (* blocks and statements: modify in place, then return true if should be kept *)
  let rec impl_block block =
    block.bstmts <- List.filter impl_stmt block.bstmts;
    block.bstmts <> []
  and impl_stmt stmt =
    (* whether the current statement should be kept, for 'if' and 'loop'
       statements this corresponds to keeping the condition node in the CFG *)
    let keep_stmt = f stmt in
    let skind'_opt = match stmt.skind with
      | If (cond, b_true, b_false, loc, exp_loc) ->
        (* filter statements (recursively) from the true and false blocks *)
        let keep_b_true, keep_b_false = impl_block b_true, impl_block b_false in
        let keep_both, keep_one = keep_b_true && keep_b_false, keep_b_true || keep_b_false in

        (* - If both the true and false blocks are to be kept, we want to keep the 'if' statement.
             In case the condition node isn't being kept (keep_stmt is false), we replace the
             condition with an unchecked placeholder (we can't remove it entirely, since we need to
             preserve control flow from the end of the true block to after the false block).
           - If at least one of the blocks is to be kept, we also keep the 'if' statement,
             replacing the other block with an empty block. *)
        if keep_both || (keep_stmt && keep_one) then
          Option.some @@ If (
            (if keep_stmt then cond else unchecked_condition ()),
            (if keep_b_true then b_true else empty_block ()),
            (if keep_b_false then b_false else empty_block ()),
            loc, exp_loc)
          (* Only one block is being kept, replace the entire 'if' statement with it. *)
        else if keep_one then
          Option.some @@ Block (if keep_b_true then b_true else b_false)
          (* Neither block is being kept. Remove entire 'if' (including condition) in this case. *)
        else
          None

      | Loop (body, loc, exp_loc, _, _) ->
        (* filter statements (recursively) from the body of the loop *)
        let keep_body = impl_block body in

        (* If the condition node is to be kept: keep the entire loop. Unlike for an 'if' statement,
           keep the loop with an empty body if just the body is to be removed, since an empty
           infinite loop is different from having nothing at all. *)
        if keep_stmt then
          Option.some @@ Loop (
            (if keep_body then body else empty_block ()),
            loc, exp_loc, None, None)
          (* If only the body is to be kept, remove the condition. Thus, the condition must really be
             dead for control flow to not change, as execution will no longer loop. *)
        else if keep_body then
          Option.some @@ Block body
        else
          None

      | Block b as skind ->
        (* Filter the statements inside the block, keep
           the block if it still contains any statements. *)
        let keep_block = impl_block b in
        if keep_stmt || keep_block then Some skind else None

      | Instr _ | Return _ | Goto _ | ComputedGoto _ | Break _ | Continue _ as skind ->
        (* no further statements are contained recursively here, so nothing left to do *)
        if keep_stmt then Some skind else None

      | Switch _ ->
        (* Handling switch statements correctly would be very difficult; consider that
           the switch labels may be located within arbitrarily nested statements within
           the switch statement's block. *)
        failwith "filter_map_block: statements must be removed"
    in

    Stdlib.Option.iter (fun skind' -> stmt.skind <- skind') skind'_opt;
    Option.is_some skind'_opt
  in
  impl_block block

(** Is it possible for this statement to begin executing normally, but not finish? *)
let may_stop_execution stmt =
  match stmt.skind with
  | Instr is -> List.exists (function Call _ | Asm _ -> true | _ -> false) is
  | _ -> false

(** Perform a depth first search over the CFG. Record the IDs of live statements;
    for each traversed edge, record the skipped statements along the edge as live,
    if the nodes on both ends of the edge are live. Record live statements in the nodes
    themselves as well. *)
let find_live_statements
    (node_live : node -> bool)
    (skipped_statements : node -> edges -> node -> stmt list)
    (cfg : cfg) (start : node)
  =
  let seen_nodes = NodeH.create 13 in
  let module IS = BatSet.Int in
  let node_id_set = function Statement stmt -> IS.singleton stmt.sid | _ -> IS.empty in

  let rec impl (n : node) (ns : node list) (live_stmts : IS.t) =
    NodeH.replace seen_nodes n ();
    let n_outbound = cfg n in

    (* If the 'from' node is dead, the statements along all outbound edges are definitely not live.
       If just the 'to' node is dead, some traversed statement along the edge stops execution;
         to be safe, mark all statements up to and including the last such statement as live.
         For example, if we have, along an edge: f() -> x += 1 -> g() -> z = 0, then f() or g() are
         not pure control-flow, so we must keep everything up to g(), but can drop z = 0.
       If both nodes are live, we keep everything along the edge. *)
    let live_stmts' =
      if node_live n then
        n_outbound
        |> List.map (fun (edges, n') ->
            let skipped = skipped_statements n edges n' in
            (if node_live n' then skipped
            (* drop after the last non-control flow statement *)
             else List.rev skipped |> BatList.drop_while (not % may_stop_execution))
            |> List.map (fun stmt -> stmt.sid)
            |> IS.of_list)
        |> List.fold_left IS.union live_stmts
        |> IS.union (node_id_set n)
      else
        live_stmts
    in

    match (n_outbound |> List.map snd |> List.filter (not % NodeH.mem seen_nodes)) @ ns with
    | [] -> live_stmts'
    | n' :: ns' -> impl n' ns' live_stmts'
  in

  impl start [] (if node_live start then node_id_set start else IS.empty)

module RemoveDeadCode : Transform.S = struct
  let transform (q : Transform.queries) (file : file) : unit =

    (* whether a global function (might) still be live, and should therefore be kept *)
    let fundec_live : fundec -> bool = not % q.must_be_uncalled in

    (* Step 1: Remove statements found to be dead. *)
    Cil.iterGlobals file
      (function
        | GFun (fd, _) ->
          (* called functions: filter statement-by-statement *)
          if fundec_live fd then
            let live_statements =
              find_live_statements
                (not % q.must_be_dead)
                q.skipped_statements
                q.cfg_forward
                (FunctionEntry fd)
            in
            filter_map_block
              ~unchecked_condition:GoblintCil.(Fun.const @@ mkString "UNCHECKED CONDITION")
              (fun stmt -> BatSet.Int.mem stmt.sid live_statements)
              fd.sbody
            |> ignore  (* ignore empty block: might be empty, but uncalled *)
            (* uncalled functions: as an optimaztion, clear the body immediately;
               if they are also unreferenced, they will be removed in the next step *)
          else
            fd.sbody <- empty_block ()
        | _ -> ());

    (* Step 2: Remove globals that are (transitively, syntactically) unreferenced by
       the main function(s). Dead functions and globals are removed, since there is no
       chain of syntactic references to them from the main function(s). *)
    let open GoblintCil.RmUnused in
    GobRef.wrap keepUnused false @@ fun () ->
    removeUnused
      ~isRoot:(function
          | GFun (fd, _) -> List.mem fd.svar.vname (get_string_list "mainfun")
          | _ -> false)
      file

  let name = "remove_dead_code"

  let requires_file_output = true

end

let _ = Transform.register (module RemoveDeadCode)
