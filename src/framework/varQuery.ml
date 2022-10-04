open GoblintCil

type t =
  | Global of CilType.Varinfo.t
  | Node of {node: Node.t; fundec: CilType.Fundec.t option}
[@@deriving ord]

type 'v f = 'v -> unit

let varinfo_from_global (g : Cil.global) : Cil.varinfo option = match g with
  | GFun (f, _) -> Some f.svar
  | GVar (v, _, _) -> Some v
  | GVarDecl (v, _) -> Some v
  | _ -> None

let varquery_from_global (g : Cil.global) : t option = match g with
  | GFun (f, _) -> Some (Node {node = FunctionEntry f; fundec = Some f})
  | GVar (v, _, _) -> Some (Global v)
  | GVarDecl (v, _) -> Some (Global v)
  | _ -> None

let varqueries_from_names (file: Cil.file) (names: string list): t list * string list =
  let module SM = Set.Make(Printable.Strings) in
  let set = SM.of_list names in

  (* Find list of [Cil.global]s that have one of the queried names, and a set of the found names *)
  let globals, matched =
    Cil.foldGlobals file (fun ((globs, matched) as acc) g ->
        match varinfo_from_global g, varquery_from_global g with
        | Some v, Some vq ->
          begin match SM.mem v.vname set  with
            | true -> (vq::globs, SM.add v.vname matched)
            | _ -> acc
          end
        | None, None -> acc
        | _, _ -> assert false
      ) ([], SM.empty) in
  (* List of queried but not found names *)
  let unmatched = List.filter (fun s -> not @@ SM.mem s matched) names in
  globals, unmatched
