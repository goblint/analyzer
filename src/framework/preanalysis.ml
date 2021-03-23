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

let makeVar fd loc name =
  let id = name ^ "__" ^ string_of_int loc.line in
  try List.find (fun v -> v.vname = id) fd.slocals
  with Not_found ->
    let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
    Goblintutil.create_var (makeLocalVar fd id ~init:(SingleInit zero) typ)

let rec list_instr_to_string l = match l with
  | [] -> ""
  | head::body -> 
  begin
    (Pretty.sprint 20 (Cil.d_instr () head))^(list_instr_to_string body)
  end

let rec get_vnames exp = match exp with
  | Lval lval -> 
    let lhost, offset = lval in 
    (match lhost with
      | Var vinfo -> vinfo.vname
      | _ -> "")
  | UnOp (unop, e, typ) -> get_vnames e
  | BinOp (binop, e1, e2, typ) -> (get_vnames e1)^" "^(get_vnames e2)
  | _ -> ""

let pairs_from_instr instr = match instr with
  | Set (lval, exp, location) ->  
    let lhost, offset = lval in
    (match lhost with
      | Var vinfo -> vinfo.vname^" <-> ["^(get_vnames exp)^"]"
      | _ -> "")
    (*" "^(Pretty.sprint 20 (Cil.d_lval () lval))^" is "^(Pretty.sprint 20 (Cil.d_exp () exp))^" | "*)
  | VarDecl (varinfo, location) -> " "^varinfo.vname^" is declared | "
  | _ -> ""

let rec get_related_pairs l = match l with
  | [] -> ""
  | head::body -> 
  begin
    (pairs_from_instr head)^(get_related_pairs body)
  end

class expressionVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
  let action s = match s.skind with
    | Instr inst -> 
      (*let () = print_endline (list_instr_to_string inst) in*)
      let () = print_endline ("Related pairs "^(get_related_pairs inst)) in
      s
    | _ -> s
  in ChangeDoChildrenPost (s, action)
end

class loopCounterVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
  let action s = match s.skind with
    | Loop (b, loc, _, _) -> 
      (* insert loop counter variable *)
      let t = var @@ makeVar fd loc "t" in
      (* initialise the loop counter to 0 *)
      let t_init = mkStmtOneInstr @@ Set (t, zero, loc) in
      (* increment the loop counter by 1 in every iteration *)
      let t_inc = mkStmtOneInstr @@ Set (t, increm (Lval t) 1, loc) in
      (match b.bstmts with
       | cont :: cond :: ss ->
         (* changing succs/preds directly doesn't work -> need to replace whole stmts  *)
         b.bstmts <- cont :: cond :: t_inc :: ss;
         let nb = mkBlock [t_init; mkStmt s.skind] in
         s.skind <- Block nb;
       | _ -> ());
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

let add_visitors = 
  let () =print_endline "Adding the visitor is called!!!" in 
  Cilfacade.register_preprocess "octApron" (new loopCounterVisitor);
  Cilfacade.register_preprocess "octApron" (new recomputeVisitor);
  Cilfacade.register_preprocess "octApron" (new expressionVisitor)