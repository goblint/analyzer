open GoblintCil

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
