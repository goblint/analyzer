(** Signatures and registry for transformations. *)

open GoblintCil
module M = Messages

(** data about analysis available to queries *)
type queries = {
  ask : ?node:Node.t -> Cil.location -> Queries.ask ;
  must_be_dead : Node.t -> bool ;
  must_be_uncalled : fundec -> bool ;

  cfg_forward : MyCFG.cfg ;
  cfg_backward : MyCFG.cfg ;
  skipped_statements : Node.t -> MyCFG.edges -> Node.t -> stmt list ;
}

module type S = sig
  val transform : queries -> file -> unit (* modifications are done in-place by CIL :( *)
  val name : string
  val requires_file_output : bool
end

let h = Hashtbl.create 13
let register (module T : S) = Hashtbl.replace h T.name (module T : S)

let run_transformations ?(file_output = true) file names ask =
  let active_transformations =
    List.filter_map
      (fun name ->
         match Hashtbl.find_opt h name with
         | Some t -> Some (name, t)
         | None -> failwith "Transformation %s does not exist!")
      names
  in

  List.iter (fun (name, (module T : S)) -> T.transform ask file) active_transformations;

  if file_output && List.exists (fun (_, (module T : S)) -> T.requires_file_output) active_transformations then
    let filename = GobConfig.get_string "trans.output" in
    Out_channel.with_open_text filename @@ fun oc ->
    GobRef.wrap GoblintCil.lineDirectiveStyle None @@ fun () ->
    dumpFile defaultCilPrinter oc filename file

let run file name = run_transformations ~file_output:false file [name]

module PartialEval = struct
  let loc = ref locUnknown (* when we visit an expression, we need the current location -> store at stmts *)
  class visitor (ask: ?node:Node.t -> Cil.location -> Queries.ask) = object
    inherit nopCilVisitor
    method! vstmt s =
      loc := Cilfacade.get_stmtLoc s;
      (* Logs.debug "Set loc at stmt %a to %a" d_stmt s CilType.Location.pretty !loc; *)
      DoChildren
    method! vexpr e =
      let eval e = match Queries.ID.to_int ((ask !loc).Queries.f (Queries.EvalInt e)) with
        | Some i ->
          let e' = integer (Z.to_int i) in
          Logs.debug "Replacing non-constant expression %a with %a at %a" d_exp e d_exp e' CilType.Location.pretty !loc;
          e'
        | None ->
          Logs.error "Can't replace expression %a at %a" d_exp e CilType.Location.pretty !loc; e
      in
      match e with
      | Const _ -> SkipChildren
      | _ -> ChangeDoChildrenPost (e, eval)
  end
  let transform q file =
    visitCilFile (new visitor q.ask) file

  let name = "partial"

  let requires_file_output = false
end
let _ = register (module PartialEval)
