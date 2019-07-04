open MyCFG
open WitnessUtil
open Graphml

module GML = DeDupGraphMlWriter (Node) (NodeGraphMlWriter (XmlGraphMlWriter))
module NH = Hashtbl.Make (Node)

let write_file filename (module Cfg:CfgBidir) (file:Cil.file) entrystates (invariant:node -> Invariant.t) (is_live:node -> bool): unit =
  let main_entry = find_main_entry entrystates in
  let loop_heads = find_loop_heads (module Cfg) file in

  let out = open_out filename in
  let g = GML.start out in

  GML.write_metadata g "witness-type" "correctness_witness";
  GML.write_metadata g "sourcecodelang" "C";
  GML.write_metadata g "producer" "Goblint";
  GML.write_metadata g "specification" "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )";
  GML.write_metadata g "programfile" (getLoc main_entry).file;

  let write_node ~entry node =
    GML.write_node g node (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin match node, invariant node with
          | Statement _, Some i ->
            [("invariant", i);
             ("invariant.scope", (getFun node).svar.vname)]
          | _ ->
            (* ignore entry and return invariants, variables of wrong scopes *)
            (* TODO: don't? fix scopes? *)
            []
        end;
        begin match node with
          | Statement s ->
            [("sourcecode", Pretty.sprint 80 (Basetype.CilStmt.pretty () s))] (* TODO: sourcecode not official? especially on node? *)

          (* violation actually only allowed in violation witness *)
          (* maybe should appear on from_node of entry edge instead *)
          | FunctionEntry f when f.vname = "__VERIFIER_error" ->
            [("violation", "true")]
          | _ -> []
        end
      ])
  in
  let write_edge from_node ((loc, edge):Cil.location * edge) to_node =
    GML.write_edge g from_node to_node (List.concat [
        begin if loc.line <> -1 then
            [("startline", string_of_int loc.line);
             ("endline", string_of_int loc.line)]
          else
            []
        end;
        begin if NH.mem loop_heads to_node then
            [("enterLoopHead", "true")]
          else
            []
        end;
        begin match from_node, to_node with
          | _, FunctionEntry f ->
            [("enterFunction", f.vname)]
          | Function f, _ ->
            [("returnFromFunction", f.vname)]
          | _, _ -> []
        end;
        begin match edge with
          (* control actually only allowed in violation witness *)
          | Test (_, b) ->
            [("control", "condition-" ^ string_of_bool b)]
          (* enter and return on other side of nodes,
             more correct loc (startline) but had some scope problem? *)
          | Entry f ->
            [("enterFunction2", f.svar.vname)]
          | Ret (_, f) ->
            [("returnFromFunction2", f.svar.vname)]
          | _ -> []
        end
      ])
  in

  let add_node ?(entry=false) node =
    write_node ~entry node
  in

  (* BFS, just for nicer ordering of witness graph children *)
  let itered_nodes = NH.create 100 in
  let rec add_edge from_node (loc, edge) to_node = match edge with
    | Proc (_, Lval (Var f, _), _) -> (* TODO: doesn't cover all cases? *)
      (* splice in function body *)
      let entry_node = FunctionEntry f in
      let return_node = Function f in
      iter_node entry_node;
      write_edge from_node (loc, edge) entry_node;
      if NH.mem itered_nodes return_node then
        write_edge return_node (loc, edge) to_node
      else
        () (* return node missing, function never returns *)
    | _ ->
      write_edge from_node (loc, edge) to_node
  and add_edges from_node locedges to_node =
    List.iter (fun locedge -> add_edge from_node locedge to_node) locedges
  and iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      add_node node;
      let locedges_to_nodes =
        Cfg.next node
        |> List.filter (fun (_, to_node) -> is_live to_node)
      in
      List.iter (fun (locedges, to_node) ->
          add_node to_node;
          add_edges node locedges to_node
        ) locedges_to_nodes;
      List.iter (fun (locedges, to_node) ->
          iter_node to_node
        ) locedges_to_nodes
    end
  in

  add_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out