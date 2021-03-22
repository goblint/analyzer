open Cfg 
open Cil
include Node

let print_filename filename =
  print_endline filename


(* Hash table of nodes that are loop headers *)
module NH = Hashtbl.Make (Node)
(* Set of visited nodes *)
module NS = Set.Make (Node)

let find_backwards_reachable (module Cfg:MyCFG.CfgBidir) (node:node): unit NH.t =
  let reachable = NH.create 100 in

  (* DFS, copied from Control is_sink *)
  let rec iter_node node =
    let () = print_endline "test" in
    let d = match node with
    | Statement stmt -> print_endline ("statement"^(Pretty.sprint 20 (Cil.d_stmt () stmt)))
    | FunctionEntry vinfo -> print_endline vinfo.vname
    | Function vinfo -> print_endline vinfo.vname
    in
    if not (NH.mem reachable node) then begin
      NH.replace reachable node ();
      List.iter (fun (_, next_node) ->
          iter_node next_node
        ) (Cfg.next node)
    end
  in

  iter_node node;
  reachable


class loopCounterVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
let () = print_endline "Visiting loop :P" in 
  let action s = match s.skind with
    | Loop (b, loc, _, _) -> 
      (* insert loop counter variable *)
      let name = "termIvana"^string_of_int loc.line in
      let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
      let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
      (* make an init stmt since the init above is apparently ignored *)
      let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc) in
      (* increment it every iteration *)
      (*let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc) in
      b.bstmts <- inc_stmt :: b.bstmts;*)
      let nb = mkBlock [init_stmt; mkStmt s.skind] in
      s.skind <- Block nb;
      s
    | _ -> s
  in ChangeDoChildrenPost (s, action)
end

class recomputeVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vfunc fd =
    computeCFGInfo fd true;
    SkipChildren
end

let do_preanalysis file =
  let () = print_endline "PREANALYSIS" in
  (*let cfgF = Hashtbl.create 113  in
  let cfgB = Hashtbl.create 113  in
  let module TmpCfg: MyCFG.CfgBidir =
    struct
      let next = Hashtbl.find_all cfgF
      let prev = Hashtbl.find_all cfgB
    end
  in
  (*let loop_heads = WitnessUtil.find_loop_heads (module TmpCfg) file in*)
  let cfgF, cfgB = MyCFG.getCFG file in
  iterGlobals file (fun glob ->
    match glob with
    | GFun (fd,loc) -> 
      let loop_heads = find_backwards_reachable (module TmpCfg) (Function fd.svar) in 
      let () = print_endline (Yojson.Safe.to_string ~std:false (node_to_yojson (Function fd.svar))) in
      let () = print_endline (Pretty.sprint 20 (Cil.d_block () fd.sbody)) in
      (*let () = print_endline (Yojson.Safe.to_string ~std:false (Cil.file_to_yojson fd.sbody)) in *)
      ()
    | _ -> ()
  ) *)
  ()

let add_visitor = 
  let () =print_endline "Adding the visitor is called!!!" in 
  Cilfacade.register_preprocess "octApron" (new loopCounterVisitor);
  Cilfacade.register_preprocess "octApron" (new recomputeVisitor)