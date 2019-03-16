open Cil
open Node
open Tracing

let store_node_location (n: node) (l: location): unit =
  NodeMap.add !location_map n l 

let loaction_of_instruction (inst: instr) = 
  match inst with
  | Set (_,_,l) -> l
  | Call (_,_,_,l) -> l
  | Asm (_,_,_,_,_,l) -> l

let location_of_instructions (instrs: instr list): location = match instrs with
  | [] -> raise (Failure "Empty list")
  | (h::t) -> loaction_of_instruction h 
    
let rec location_of_statement (s: stmt) = match s.skind with
  | Instr is -> location_of_instructions is
  | Return (_,l) -> l
  | Goto (_,l) -> l
  | ComputedGoto (_,l) -> l
  | Break l -> l
  | Continue l -> l
  | If (_,_,_,l) -> l
  | Switch (_,_,_,l) -> l
  | Loop (_,l,_,_) -> l
  | Block b -> location_of_statement (List.hd b.bstmts)
  | TryFinally (_,_,l) -> l
  | TryExcept (_,_,_,l) -> l

let update_ids (old_file: file) (new_file: file) (map: (string, Cil.global * VersionLookup.commitID) Hashtbl.t) (current_commit: string) (already_analyzed: bool) =
  let vid_max = ref (Cil.newVID ()) in
  let sid_max = ref (-1) in

  let update_vid_max vid =
    if vid > !vid_max then vid_max := vid
  in

  let update_sid_max sid =
    if sid > !sid_max then sid_max := sid
  in

  let make_vid () = 
    vid_max := !vid_max +1;
    !vid_max
  in
  let make_sid () = 
    sid_max := !sid_max +1;
    !sid_max
  in
  let override_fundec (target: fundec) (src: fundec) = 
    target.sbody <- src.sbody;
    target.sallstmts <- src.sallstmts;
    target.sformals <- src.sformals;
    target.slocals <- src.slocals;
    target.smaxid <- src.smaxid;
    target.smaxstmtid <- src.smaxstmtid;
    target.svar <- src.svar;  
  in
  print_endline @@ "Commit has already been analyzed? : " ^ string_of_bool already_analyzed;
  let reset_fun (f: fundec) (old_f: fundec) =
      f.svar.vid <- old_f.svar.vid;
      List.iter (fun (l, o_l) -> l.vid <- o_l.vid) (List.combine f.slocals old_f.slocals);
      List.iter (fun (lo, o_f) -> lo.vid <- o_f.vid) (List.combine f.sformals old_f.sformals);
      List.iter (fun (s, o_s) -> s.sid <- o_s.sid) (List.combine f.sallstmts old_f.sallstmts);
      List.iter (fun s -> store_node_location (Statement s) (location_of_statement s)) f.sallstmts;

      List.iter (fun l -> update_vid_max l.vid) f.slocals;
      List.iter (fun f -> update_vid_max f.vid) f.sformals;
      List.iter (fun s -> update_sid_max s.sid) f.sallstmts;
      override_fundec f old_f;
      update_vid_max f.svar.vid;
  in
  let reset_var (v: varinfo) (old_v: varinfo)=
    v.vid <- old_v.vid;
    update_vid_max v.vid;
  in
  let reset_globals (glob: global) =
    try
      let (old_glob, commit) = Hashtbl.find map (CompareAST.name_of_global glob) in
      if already_analyzed || not (String.equal commit current_commit) then (
      match (glob, old_glob) with 
      | GFun (nw, _), GFun (old, _) -> reset_fun nw old
      | GVar (nw, _, _), GVar (old, _, _) -> reset_var nw old
      | GVarDecl (nw, _), GVarDecl (old, _) -> reset_var nw old
      | _ -> ())
    with Failure m -> ()  
  in
  let update_fun (f: fundec) (old_f: fundec) =
      print_endline @@ "updating " ^ old_f.svar.vname;
      f.svar.vid <- make_vid ();
      List.iter (fun l -> l.vid <- make_vid ()) f.slocals;
      List.iter (fun f -> f.vid <- make_vid ()) f.sformals;
      List.iter (fun s -> s.sid <- make_sid ()) f.sallstmts;
  in
  let update_var (v: varinfo) (old_v: varinfo) =
    v.vid <- make_vid ()
  in
  let update_globals (glob: global) = 
    try
      let (old_glob, commit) = Hashtbl.find map (CompareAST.name_of_global glob) in
      if (String.equal commit current_commit) then (
        match (glob, old_glob) with 
        | GFun (nw, _), GFun (old, _) -> update_fun nw old
        | GVar (nw, _, _), GVar (old, _, _) -> update_var nw old
        | GVarDecl (nw, _), GVarDecl (old, _) -> update_var nw old
        | _ -> ())
    with Failure m -> ()
  in
  let print_fun_ids (f: fundec) =
      print_endline (f.svar.vname ^ ": " ^ string_of_int f.svar.vid);
      List.iter (fun l -> print_endline @@ "local: " ^string_of_int l.vid) f.slocals;
      List.iter (fun f -> print_endline @@ "formal: " ^string_of_int  f.vid) f.sformals;
      List.iter (fun s -> print_endline @@ "stmt_id: " ^string_of_int  s.sid) f.sallstmts;
  in
  let print_globals (glob: global) = match glob with
    | GFun (fn, loc) -> print_fun_ids fn
    | GVar (v, _, _) -> print_endline (v.vname ^ ": " ^ string_of_int v.vid);
    | GVarDecl (v, _ ) -> print_endline (v.vname ^ ": " ^ string_of_int v.vid);
    | _ -> ()
  in
   
(* 
  print_endline "Global IDs!!";
  iterGlobals new_file print_globals;
  print_endline "After :"; *)
  iterGlobals new_file reset_globals;
  if not already_analyzed then iterGlobals new_file update_globals;
(*   iterGlobals new_file print_globals;
 *)
  print_endline "globinit";
  match new_file.globinit with
  | Some f -> print_endline f.svar.vname;
  | None -> print_endline "Nix";
  
  while newVID () < 3000 do
    ()
  done;

  ()
    
  