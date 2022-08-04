open MyCFG

module NH = Hashtbl.Make (Node)
module NS = Set.Make (Node)

let find_main_entry entrystates =
  let (main_entry_nodes, other_entry_nodes) =
    entrystates
    |> List.map fst
    |> List.partition (function
        | FunctionEntry f, _ -> f.svar.vname = "main"
        | _, _ -> false
      )
  in
  match main_entry_nodes, other_entry_nodes with
  | [], _ -> failwith "no main_entry_nodes"
  | _ :: _ :: _, _ -> failwith "multiple main_entry_nodes"
  | _, _ :: _ -> failwith "some other_entry_nodes"
  | [main_entry], [] -> main_entry

let find_loop_heads (module Cfg:CfgForward) (file:Cil.file): unit NH.t =
  let loop_heads = NH.create 100 in
  let global_visited_nodes = NH.create 100 in

  (* DFS *)
  let rec iter_node path_visited_nodes node =
    if NS.mem node path_visited_nodes then
      NH.replace loop_heads node ()
    else if not (NH.mem global_visited_nodes node) then begin
      NH.replace global_visited_nodes node ();
      let new_path_visited_nodes = NS.add node path_visited_nodes in
      List.iter (fun (_, to_node) ->
          iter_node new_path_visited_nodes to_node
        ) (Cfg.next node)
    end
  in

  Cil.iterGlobals file (function
      | GFun (fd, _) ->
        let entry_node = FunctionEntry fd in
        iter_node NS.empty entry_node
      | _ -> ()
    );

  loop_heads


module type File =
sig
  val file: Cil.file
end

module Invariant (File: File) (Cfg: MyCFG.CfgBidir) =
struct
  let emit_loop_head = GobConfig.get_bool "witness.invariant.loop-head"
  let emit_after_lock = GobConfig.get_bool "witness.invariant.after-lock"
  let emit_other = GobConfig.get_bool "witness.invariant.other"

  let loop_heads = find_loop_heads (module Cfg) File.file

  let is_after_lock to_node =
    List.exists (fun (edges, from_node) ->
        List.exists (fun (_, edge) ->
            match edge with
            | Proc (_, Lval (Var fv, NoOffset), args) ->
              let desc = LibraryFunctions.find fv in
              begin match desc.special args with
                | Lock _ -> true
                | _ -> false
              end
            | _ -> false
          ) edges
      ) (Cfg.prev to_node)

  let is_invariant_node cfgnode =
    if NH.mem loop_heads cfgnode then
      emit_loop_head
    else if is_after_lock cfgnode then
      emit_after_lock
    else
      emit_other
end

module InvariantExp =
struct
  module ES = SetDomain.Make (CilType.Exp)

  (* Turns an expression into alist of conjuncts, pulling out common conjuncts from top-level disjunctions *)
  let rec pullOutCommonConjuncts e =
    let rec to_conjunct_set = function
      | Cil.BinOp(LAnd,e1,e2,_) -> ES.join (to_conjunct_set e1) (to_conjunct_set e2)
      | e -> ES.singleton e
    in
    let combine_conjuncts es = ES.fold (fun e acc -> match acc with | None -> Some e | Some acce -> Some (BinOp(LAnd,acce,e,Cil.intType))) es None in
    match e with
    | Cil.BinOp(LOr, e1, e2,t) ->
      let e1s = pullOutCommonConjuncts e1 in
      let e2s = pullOutCommonConjuncts e2 in
      let common = ES.inter e1s e2s in
      let e1s' = ES.diff e1s e2s in
      let e2s' = ES.diff e2s e1s in
      (match combine_conjuncts e1s', combine_conjuncts e2s' with
       | Some e1e, Some e2e -> ES.add (BinOp(LOr,e1e,e2e,Cil.intType)) common
       | _ -> common (* if one of the disjuncts is empty, it is equivalent to true here *)
      )
    | e -> to_conjunct_set e

  let process_exp inv =
    let inv' = InvariantCil.exp_replace_original_name inv in
    if GobConfig.get_bool "witness.invariant.split-conjunction" then
      ES.elements (pullOutCommonConjuncts inv')
    else
      [inv']
end
