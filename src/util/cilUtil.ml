(** Functionality to create [VarQuery.t] lists from a list of names of global variable and function definitions *)
(* Separate file from cilfacade.ml, for functions that depend both on [Node] and [VarQuery]. Avoids dependency cycle *)

module type S =
sig
  val varquery_from_names : Cil.file -> string list -> VarQuery.t list
end

module M : S =
struct
  let varinfo_from_global (g : Cil.global) : Cil.varinfo option = match g with
    | GFun (f, _) -> Some f.svar
    | GVar (v, _, _) -> Some v
    | GVarDecl (v, _) -> Some v
    | _ -> None

  let varquery_from_global (g : Cil.global) : VarQuery.t option = match g with
    | GFun (f, _) -> Some (VarQuery.Node {node = FunctionEntry f; fundec = Some f})
    | GVar (v, _, _) -> Some (VarQuery.Global v)
    | GVarDecl (v, _) -> Some (VarQuery.Global v)
    | _ -> None

  (** Takes a [Cil.file] and a list of names of globals, and returns a
      list of [VarQuery.t]s of globals whose [vname] is contained in the argument list.
      For  *)
  let varquery_from_names (file: Cil.file) (names: string list): VarQuery.t list =
    let module SM = Set.Make(Printable.Strings) in
    let set = SM.of_list names in
    let globals =
      Cil.foldGlobals file (fun acc g ->
          match varinfo_from_global g, varquery_from_global g with
          | Some v, Some vq ->
            begin match SM.mem v.vname set  with
              | true -> vq::acc
              | _ -> acc
            end
          | None, None -> acc
          | _, _ -> assert false
        ) [] in
    globals
end

include M
