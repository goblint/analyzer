open Cfg 
open Cil
include Node

let print_filename filename =
  print_endline filename

(* Hash table of nodes that are loop headers *)
module NH = Hashtbl.Make (Node)
(* Set of visited nodes *)
module NS = Set.Make (Node)
(*let find_variables (module Cfg:CfgForward) cfgF (fd:Cil.fundec) = 
  let loop_heads = NH.create 100 in
  let global_visited_nodes = NH.create 100 in

  let () = print_endline fd.svar.vname in

  (* DFS *)
  let rec iter_node path_visited_nodes node =
    if NS.mem node path_visited_nodes then
      NH.add loop_heads node ()
    else if not (NH.mem global_visited_nodes node) then begin
      NH.add global_visited_nodes node ();
      let new_path_visited_nodes = NS.add node path_visited_nodes in
      List.iter (fun (_, to_node) ->
          iter_node new_path_visited_nodes to_node
        ) (Cfg.next node)
    end
  in
  let entry_node = FunctionEntry fd.svar in
  iter_node NS.empty entry_node;

  ()  

let do_preanalysis file =
  let cfgF, cfgB = MyCFG.getCFG file in
  iterGlobals file (fun glob ->
    match glob with
    | GFun (fd,loc) -> 
      let module TmpCfg: MyCFG.CfgBidir =
      struct
        let next = Hashtbl.find_all cfgF
        let prev = Hashtbl.find_all cfgB
      end
      in
      let loop_heads = find_variables (module TmpCfg) fd in
      find_variables cfgF fd
    | _ -> ()
  ) *)