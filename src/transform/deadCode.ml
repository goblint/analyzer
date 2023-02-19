open GoblintCil
open GobConfig

(* let f = Printf.sprintf
let pf fmt = Printf.ksprintf print_endline fmt
let df fmt = Pretty.gprintf (Pretty.sprint ~width:max_int) fmt
let dpf fmt = Pretty.gprintf (fun doc -> print_endline @@ Pretty.sprint ~width:max_int doc) fmt *)

(* what to do about goto to removed statements? probably just check if the target of a goto should be removed, if so remove the goto? <- don't do that.
   but if the target is not live, the goto can't be live anyway *)

(** Filter statements out of a block (recursively). CFG fields (prev, next, etc.) are no longer valid after calling.
    Invariants:
    - f (goto label) ==> f (labelled stmt), i.e. if a goto statement is not filtered out, the target may not be filtered out either.
    - block may not contain switch statements *)
let filter_map_block f (block : Cil.block) : bool =
  (* blocks and statements: modify in place, then return true if should be kept *)
  let rec impl_block block =
    block.bstmts <- List.filter impl_stmt block.bstmts;
    block.bstmts <> []
  and impl_stmt stmt =
    if not (f stmt) then false
    else
      let skind', keep =
      (* TODO: if sk is not changed in the end, simplify here *)
        match (stmt.skind : stmtkind) with
        | If (_, b1, b2, _, _) as sk ->
          (* be careful to not short-circuit, since call to impl_block b2 is always needed for side-effects *)
          sk, let keep_b1, keep_b2 = impl_block b1, impl_block b2 in keep_b1 || keep_b2
        | Switch _ -> failwith "switch statements must be removed"
          (* handling switch statements correctly would be very difficult; consider that the switch
          labels may be located within arbitrarily nested statements within the switch statement's block
          TODO: are switch statements always removed by goblint/CIL? *)
          (* TODO: block and stmt list in Switch: each stmt in list points to the first statement for a case, filtering twice seems silly, and is probably not even correct!
          Instead, we should filter the block, and then pick out the stmt for each case. *)
        (* | Switch (e, b, stmts, l1, l2) -> *)
          (* let keep_b = impl_block b in
          let stmts' = List.filter impl_stmt stmts in
          Switch (e, b, stmts', l1, l2), keep_b || stmts' <> [] *)
        | Loop (b, _, _, _, _) as sk ->
          sk, impl_block b
        | Block b as sk ->
          sk, impl_block b
        | sk -> sk, true
      in
      stmt.skind <- skind';
      keep
    in
    impl_block block

(* globinfo: the type of globals between which we want to track dependencies *)

type globinfo =
  | GTypeInfo of (typeinfo [@equal (==)] [@hash Hashtbl.hash])
  | GCompInfo of (compinfo [@equal (==)] [@hash Hashtbl.hash])
  | GEnumInfo of (enuminfo [@equal (==)] [@hash Hashtbl.hash])
  | GVarInfo of (varinfo [@equal (==)] [@hash Hashtbl.hash])
  [@@deriving eq, hash]

let pretty_globinfo () = let open Pretty in function
  | GTypeInfo ti -> text "GTypeInfo " ++ text ti.tname
  | GCompInfo ci -> text "GCompInfo " ++ text ci.cname
  | GEnumInfo ei -> text "GEnumInfo " ++ text ei.ename
  | GVarInfo vi -> text "GVarInfo " ++ text vi.vname

module GlobinfoH =
  Hashtbl.Make
    (struct
      type t = globinfo
      let equal = equal_globinfo
      let hash = hash_globinfo
    end)

let globinfo_of_global = function
  | GType (ti, _) -> Some (GTypeInfo ti)
  | GCompTag (ci, _) | GCompTagDecl (ci, _) -> Some (GCompInfo ci)
  | GEnumTag (ei, _) | GEnumTagDecl (ei, _) -> Some (GEnumInfo ei)
  | GVarDecl (vi, _) | GVar (vi, _, _) | GFun ({ svar = vi; _ }, _) -> Some (GVarInfo vi)
  | _ -> None

class globalReferenceTrackerVisitor = object (self)
  inherit Cil.nopCilVisitor (* as nop *)

  (** map of globals to the set of globals they reference *)
  val glob_refs : (unit GlobinfoH.t) GlobinfoH.t = GlobinfoH.create 17

  method get_references_raw () = glob_refs
  method get_references () = GlobinfoH.to_seq glob_refs |> Seq.map (fun (k, v) -> k, GlobinfoH.to_seq_keys v)

  (** context is the global we are currently iterating within *)
  val context : global option ref = ref None

  (** mark [glob_from] as referencing [glob_to] *)
  method private add_ref glob_from glob_to =
    let open GlobinfoH in
    let ref_set =
      match find_opt glob_refs glob_from with 
      | None -> create 3
      | Some s -> s
    in
    replace ref_set glob_to ();
    replace glob_refs glob_from ref_set

  method private ctx_add_ref glob_to =
    Option.bind !context globinfo_of_global
    |> Option.iter (fun ctx -> self#add_ref ctx glob_to)

  (* TODO: is the typeinfo in a global traversed? looks like yes *)
  method! vglob g =
    (* upon entering a new global, update the context *)
    context := Some g;
    DoChildren

  method! vvrbl vi =
    (* if variable is global, add reference from current context *)
    if vi.vglob then self#ctx_add_ref (GVarInfo vi);
    DoChildren

  method! vtype t =
    (match t with
    | TNamed (ti, _) -> self#ctx_add_ref @@ GTypeInfo ti
    | TComp (ci, _) -> self#ctx_add_ref @@ GCompInfo ci
    | TEnum (ei, _) -> self#ctx_add_ref @@ GEnumInfo ei
    | _ -> ());
    DoChildren

end


let find_live_globinfo (live_from : global Seq.t) (references : globinfo -> globinfo Seq.t) =
  let live = GlobinfoH.create 103 in
  let rec impl = function
    | [] -> ()
    | gi :: gis ->
      GlobinfoH.replace live gi ();
      let new_refs =
        references gi
        |> Seq.filter (fun rgi -> not (GlobinfoH.mem live rgi))
        |> List.of_seq
      in
      impl (List.rev_append new_refs gis)
  in
  impl (live_from |> Seq.filter_map globinfo_of_global |> List.of_seq);
  live

let find_live_globinfo' live_from result =
  find_live_globinfo
    live_from
    (fun gi ->
      GlobinfoH.find_opt result gi
      |> Option.to_seq
      |> Seq.concat_map GlobinfoH.to_seq_keys)


module type DeadCodeArgs = sig
  val stmt_live : stmt -> bool
  val fundec_live : fundec -> location -> bool
end

module RemoveDeadCode (A : DeadCodeArgs) : Transform.S = struct
  let transform (ask : ?node:Node.t -> Cil.location -> Queries.ask) (file : file) : unit =

    (* step 1: remove statements found to be dead *)
    Cil.iterGlobals file
      (function
      | GFun (fd, _) -> filter_map_block A.stmt_live fd.sbody |> ignore
        (* pf "global name=%s" fd.svar.vname;
        let keep =  in
        pf "keep=%b" keep; *) (* TODO: use keep? discard function if keep is false. should not be necessary, function should be dead already *)
      | _ -> ());

    let global_live ~non_functions_live = function
      | GFun (fd, l) -> A.fundec_live fd l
      | _ -> non_functions_live
    in

    if get_bool "dbg.cil_dead_glob" then (
      let open GoblintCil.Rmtmps in
      (* dpf "using cil to remove dead globals, keepUnused=%b" !keepUnused; *)
      let keepUnused0 = !keepUnused in
      Fun.protect ~finally:(fun () -> keepUnused := keepUnused0) (fun () ->
        keepUnused := false;
        removeUnusedTemps (* ~isRoot:isCompleteProgramRoot *) ~isRoot:(global_live ~non_functions_live:false) file
      )
      (* let open GoblintCil in
      let open Rmtmps in
      Rmtmps.clearReferencedBits file;
      Cfg.cfgFun |> ignore *)
      (* GoblintCil.Cfg.clearFileCFG file;
      GoblintCil.Cfg.clearCFGinfo *)
    )
    else (
      print_endline "using custom code to remove dead globals";

      (* step 2: remove function globals found to be dead *)
      file.globals <- List.filter (global_live ~non_functions_live:true) file.globals;

      (* step 3: track dependencies between globals *)
      let refsVisitor = new globalReferenceTrackerVisitor in
      Cil.visitCilFileSameGlobals (refsVisitor :> Cil.cilVisitor) file;

      (* step 4: find globals referenced by remaining (live) functions and remove them *)
      let live_globinfo =
        find_live_globinfo'
          (file.globals |> List.to_seq |> Seq.filter (function GFun _ -> true | _ -> false))
          (refsVisitor#get_references_raw ())
      in
      file.globals <-
        List.filter
          (fun g -> match globinfo_of_global g with
          | Some gi -> GlobinfoH.mem live_globinfo gi
          | None -> true (* dependencies for some types of globals (e.g. assembly) are not tracked, always keep them *)
          )
        file.globals
    )
  let requires_file_output = true

end
