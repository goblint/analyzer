(* 
  Simplified node and edge structures or vilation witness generation on local traces   
*)
open GobConfig
open GoblintCil
open Graphml
open LocalTraces

module ViolationWitness = 
struct

let escape = XmlUtil.escape

let output_file = "output.txt"
let default_violation_filename = "violation-witness.graphml"
let violation_filename_parameter = "witness.violation.filename"
let violation_unreach_functions_parameter = "witness.violation.unreach-functions"
let default_violation_unreach_functions = ["reach_error"]

let local_traces_specific_prefix = "__goblint__traces__"
let local_traces_specific_prefix_regexp = Str.regexp_string local_traces_specific_prefix
let local_traces_pthread_func_prefix = "pthread_mutex_"


(* Get all edges with avoiding Skip and making connections avoiding some edges like Skip and custom mutex lock/unlock for global variables *)
let get_local_trace_edges graph =
  let skip_edge:(CustomEdge.t) = Skip in
  let filterOutEdge e = 
    let edge:(CustomEdge.t) = e in
    match edge with 
      | Skip -> true
      (* avoiding custom-generated assign edges like __goblint__traces__XXXX = XXXX *)
      | Assign((Var(info), _), _) -> String.starts_with ~prefix:local_traces_specific_prefix info.vname  
      (* avoiding custom-generated edges like pthread_mutex_unlock(&__goblint__traces__XXXX) *)
      | Proc(None, Lval(Var(funcInfo), NoOffset),[AddrOf(Var(addrArg), NoOffset)]) -> (String.starts_with ~prefix:local_traces_pthread_func_prefix funcInfo.vname) && (String.starts_with ~prefix:local_traces_specific_prefix addrArg.vname) 
      | DepMutex(_) -> true
      | _ -> false
  in
  let rec buildPairs g = match g with 
    | (v1, e, v2)::subG -> if subG==[] then (v1, e)::(v2, skip_edge)::[] else (v1, e)::(buildPairs subG)
    | [] -> []
  in
  let rec foldPairs g = match g with 
    | [] -> []
    | (v, e)::subG ->
        if filterOutEdge e then
          let sub = foldPairs subG in 
          match sub with 
            | [] -> [(v, e)] 
            | _ -> sub
        else (v, e)::(foldPairs subG)  
  in      
  let rec gatherPairs g = match g with 
    | [] -> [] 
    | (v1, e1)::subG -> 
          match subG with 
            | (v2, e2)::_ -> (v1, e1, v2)::(gatherPairs subG)
            | _ -> [] 
  in  
  let rec entryMainPresent g = match g with 
    | [] -> false
    | (v1, e, v2)::rest ->
        let edge:(CustomEdge.t) = e in 
        match edge with 
          | Entry(f) -> if (f.svar.vname="main") then true else entryMainPresent rest  
          | _ -> entryMainPresent rest
  in
  let rec skipTillMain g = match g with 
    | [] -> []
    | (v1, e, v2)::rest ->
        let edge:(CustomEdge.t) = e in 
        match edge with 
          | Entry(f) -> if (f.svar.vname="main") then g else skipTillMain rest  
          | _ -> skipTillMain rest
  in
  let allEdges = LocalTrace.get_all_edges graph in
  (if entryMainPresent allEdges then skipTillMain allEdges else allEdges) |> buildPairs |> foldPairs |> gatherPairs;;
  (*let rec build g = match g with 
    | (v1, e, v2)::subG -> 
        if e=skip_edge then 
          let sub = build subG in 
          match sub with 
            | (subV1, subE, subV2)::rest -> (v1, subE, subV2)::rest
            | [] -> []
        else (v1, e, v2)::(build subG)    
    | [] -> []        
  in
  build (LocalTrace.get_all_edges graph);; *)

  (* enhance and squeeze (for case of entry function and dependency mutex) list of edges 
      from 3-tuple (node-from, edge, node-to) 
      to 5-tuple (node-from, edge, node-to, context-function-list, entry-function-here) 
     - adding the context of called at this node functions names *)
  let add_stack_to_edges edges =
    let rec build funcs edges = 
        match edges with 
        | (v1, e, v2)::rest ->
              let edge:(CustomEdge.t) = e in
              let newFuncs = 
                match edge with
                | Entry (f) -> (f.svar.vname)::funcs
                | Ret (_, f) -> if funcs==[] then [] else (List.tl funcs)
                | _ -> funcs
              in
              (v1, e, v2, funcs)::(build newFuncs rest)   
        | [] -> [] 
      in
      let rec squeezeEntryFunc edges = 
        match edges with 
          | (v1, e1, v2, funcs)::rest ->
            let edge:(CustomEdge.t) = e1 in
              (match rest with 
                | (nv1, Entry(f), nv2, nextFuncs)::restrest -> (v1, edge, nv2, funcs, f.svar.vname)::(squeezeEntryFunc restrest)
                (* vInfo is not used now - but may be later it's needed for threadId, createThread parameters *)
                | (nv1, DepMutex(vInfo), nv2, nextFuncs)::restrest -> (v1, edge, nv2, funcs, "")::(squeezeEntryFunc restrest)
                | _ -> (v1, edge, v2, funcs, "")::(squeezeEntryFunc rest)
              )
          | [] -> []
      in      
      squeezeEntryFunc (build [] edges)           
  ;; 
  
  let get_violation_description (spec:(SvcompSpec.t)) node =
    match spec with
      | SvcompSpec.UnreachCall f -> "unreach-call: " ^ f ^ "();"
      | SvcompSpec.NoDataRace -> "no-data-race"
      | SvcompSpec.NoOverflow -> "no-overflow" ^ ""
  ;;
  let get_last_not_error_node graph = 
    let rec find g = 
        match g with 
          | [] -> None
          | (v1, e, v2)::rest -> 
              let last = find rest in 
              if Option.is_some last 
                then last
                else if (v2.programPoint==LocalTrace.error_node) then None else Some v2 
    in            
    find (LocalTrace.get_all_edges graph)
  ;;  

  let get_unreach_functions () = 
    try get_string_list violation_unreach_functions_parameter with Failure _ -> default_violation_unreach_functions
  ;;

  let check_function_for_unreachable funcName = 
    Option.is_some (List.find_opt (fun fName -> fName=funcName) (get_unreach_functions ()))
  ;;
let create_witness graph violation_type = 
    (*Violation witness header*)
    let module GML = XmlGraphMlWriter in
    let defParamFileName = try get_string violation_filename_parameter with Failure _ -> "" in
    let out = open_out (if defParamFileName=="" then default_violation_filename else defParamFileName) in
    let g = GML.start out in

    (*parameters*)
    GML.write_key g "graph" "witness-type" "string" None;
    GML.write_key g "graph" "sourcecodelang" "string" None;
    GML.write_key g "graph" "producer" "string" None;
    GML.write_key g "graph" "specification" "string" None;
    GML.write_key g "graph" "programfile" "string" None;
    GML.write_key g "graph" "programhash" "string" None;
    GML.write_key g "graph" "architecture" "string" None;
    GML.write_key g "graph" "creationtime" "string" None;
    GML.write_key g "node" "entry" "boolean" (Some "false");
    GML.write_key g "node" "sink" "boolean" (Some "false");
    GML.write_key g "node" "violation" "boolean" (Some "false");
    GML.write_key g "node" "violatedProperty" "string" None;
    GML.write_key g "edge" "assumption" "string" None;
    GML.write_key g "edge" "assumption.scope" "string" None;
    GML.write_key g "edge" "assumption.resultfunction" "string" None;
    GML.write_key g "edge" "control" "string" None;
    GML.write_key g "edge" "startline" "int" None;
    GML.write_key g "edge" "endline" "int" None;
    GML.write_key g "edge" "startoffset" "int" None;
    GML.write_key g "edge" "endoffset" "int" None;
    GML.write_key g "edge" "enterLoopHead" "boolean" (Some "false");
    GML.write_key g "edge" "enterFunction" "string" None;
    GML.write_key g "edge" "returnFromFunction" "string" None;
    GML.write_key g "edge" "threadId" "string" None;
    GML.write_key g "edge" "createThread" "string" None;

    (*start graph*)
    GML.start_graph g;

    GML.write_metadata g "witness-type" "violation_witness";
    GML.write_metadata g "sourcecodelang" "C";
    GML.write_metadata g "producer" (Printf.sprintf "Goblint (%s)" Version.goblint);
    GML.write_metadata g "specification" (Svcomp.Specification.to_string violation_type);
    let lastNotErrorNode = get_last_not_error_node graph in 
    let programfile = if (Option.is_none lastNotErrorNode) 
                      then "-UNKNONN-FILE-"  
                      else (Node.location (Option.get lastNotErrorNode).programPoint).file 
    in
    GML.write_metadata g "programfile" programfile;
    let programhash = if (Option.is_none lastNotErrorNode) then "" else Sha256.(to_hex (file programfile)) in
    GML.write_metadata g "programhash" programhash;
    GML.write_metadata g "architecture" (get_string "exp.architecture");
    GML.write_metadata g "creationtime" (TimeUtil.iso8601_now ());

    (*nodes and edges*)
    let getId node = "N" ^ (string_of_int node.id) 
    in
    let make_assumption testExp conditionRes = 
        let logicalExp = 
          match testExp with
            | BinOp(op, _, _, typ) -> 
              (match op with |Lt |Gt |Le |Ge |Eq |Ne |LAnd |LOr -> true | _ -> false)
            | _ -> false 
        in
        let strExp = Str.global_replace local_traces_specific_prefix_regexp "" (CilType.Exp.show testExp) in
        if logicalExp 
          then if conditionRes then strExp else  "!("^strExp^")"
          else if conditionRes then "("^strExp^")!=0" else "("^strExp^")==0"      
    in  
    let write_node ?(violation=false) ?(entry=false) node = 
      GML.write_node g (getId node) 
                      (List.concat [
                        begin if entry then [("entry", "true")] else [] end;
                        begin if violation 
                              then [("violation", "true"); 
                                    ("violatedProperty", (get_violation_description violation_type node)) (*add line of violation*)
                                   ] 
                              else [] 
                        end;
                      ]); 
    in 
    let write_edge edge contextFunc entryFunc = match edge 
          with (v1, ed, v2) -> 
            let e:(CustomEdge.t) = ed in
            let unsqueezeEntry, entryFunction = match e with | Entry (f) -> (true, f.svar.vname) | _ -> (false, entryFunc) in 
            let returnFunction = match e with | Ret (_, f) -> f.svar.vname | _ -> "" in 
            let testCondition, testExp, conditionRes = match e with | Test (exp, cond) -> (true, exp, cond) | _ -> (false, Const(CChr 'a'), false) in 
            let isEntry = (String.length entryFunction)>0 in 
            let isReturn = (String.length returnFunction)>0 in 
            let isContextFunc = (String.length contextFunc)>0 in
            GML.write_edge g (getId v1) (getId v2) 
                          (List.concat [
                            begin let loc = Node.location v1.programPoint in
                              (* exclude line numbers from sv-comp.c and unknown line numbers *)
                              if loc.file = programfile && loc.line <> -1 then
                                [ ("startline", string_of_int loc.line); 
                                  ("endline", string_of_int loc.line) ]
                              else
                                []
                            end;
                            [("local-trace-line", EdgeImpl.show e)];
                            begin if ((not unsqueezeEntry) && (* not isEntry && *) not isReturn)
                                  then [("assumption", 
                                          if (testCondition) 
                                            then make_assumption testExp conditionRes
                                            else "1")
                                       ]
                                  else []
                            end;      
                            begin if (testCondition)
                                  then [("control", if (conditionRes) then "condition-true" else "condition-false")]
                                  else []  
                            end;
                            begin if (isContextFunc && (* not isEntry && *) not isReturn)
                                  then ["assumption.scope", contextFunc] 
                                  else [] 
                            end;
                            begin if isEntry then ["enterFunction", entryFunction] else [] 
                            end;
                            begin if isReturn then ["returnFromFunction", returnFunction] else [] 
                            end;
                          ]); 
    in
    let traceEdges = add_stack_to_edges (get_local_trace_edges graph) in
    let entryNode = match traceEdges with 
                      | (v1, _, _, _, _)::_ -> Some v1
                      | [] -> None
    in                  
    List.iter 
      (fun edge -> 
        match edge with (v1, ed, v2, funcs, entryFunc) ->
            let violation = v2.programPoint==LocalTrace.error_node in
            let entry = if Option.is_some entryNode then (Option.get entryNode)==v1 else false in
              write_node v1 ~entry:entry;
              write_edge (v1, ed, v2) (if funcs==[] then "" else (List.hd funcs)) entryFunc;
              if violation then 
                (write_node v2 ~violation:true);   
      ) 
      traceEdges;
    
    GML.stop g;
    close_out_noerr out;;

end