open MyCFG

module M = Messages

module type BiArg =
sig
  include MyARG.S with module Edge = MyARG.InlineEdge

  val prev: Node.t -> (Edge.t * Node.t) list
  val iter_nodes: (Node.t -> unit) -> unit

  val query: Node.t -> 'a Queries.t -> 'a Queries.result
end

module Dot (Arg: BiArg) =
struct
  let dot_node_name ppf node =
    Format.fprintf ppf "\"%s\"" (Arg.Node.to_string node)

  let dot_edge ppf from_node (edge, to_node) =
    Format.fprintf ppf "@,%a -> %a [label=\"%s\"];" dot_node_name from_node dot_node_name to_node (String.escaped (Arg.Edge.to_string edge))

  let dot_node ppf node =
    let shape = match Arg.Node.cfgnode node with
      | Statement {skind=If (_,_,_,_,_); _}  -> "diamond"
      | Statement _     -> "oval"
      | Function _
      | FunctionEntry _ -> "box"
    in
    Format.fprintf ppf "@,%a [shape=%s];" dot_node_name node shape;
    List.iter (dot_edge ppf node) (Arg.next node)

  let dot_nodes ppf =
    Arg.iter_nodes (dot_node ppf)

  let dot ppf =
    Format.fprintf ppf "@[<v 2>digraph arg {%t@]@,}@\n" dot_nodes
end

let current_arg: (module BiArg) option ref = ref None

module Make (R: ResultQuery.SpecSysSol2) =
struct
  open R
  open SpecSys

  module Query = ResultQuery.Query (SpecSys)

  let get: node * Spec.C.t -> Spec.D.t =
    fun nc -> LHT.find_default lh nc (Spec.D.bot ())

  let ask_indices lvar =
    let indices = ref [] in
    ignore (ask_local lvar (Queries.IterVars (fun i ->
        indices := i :: !indices
      )));
    !indices

  module CfgNode = Node

  module Node =
  struct
    type t = MyCFG.node * Spec.C.t * int

    let equal (n1, c1, i1) (n2, c2, i2) =
      EQSys.LVar.equal (n1, c1) (n2, c2) && i1 = i2

    let compare (n1, c1, i1) (n2, c2, i2) =
      let r = EQSys.LVar.compare (n1, c1) (n2, c2) in
      if r <> 0 then
        r
      else
        Int.compare i1 i2

    let hash (n, c, i) = 31 * EQSys.LVar.hash (n, c) + i

    let cfgnode (n, c, i) = n
    let context_id (n, c, i) = Spec.C.tag c
    let path_id (n, c, i) = i

    let to_string (n, c, i) =
      (* copied from NodeCtxStackGraphMlWriter *)
      let c_tag = Spec.C.tag c in
      let i_str = string_of_int i in
      match n with
      | Statement stmt  -> Printf.sprintf "s%d(%d)[%s]" stmt.sid c_tag i_str
      | Function f      -> Printf.sprintf "ret%d%s(%d)[%s]" f.svar.vid f.svar.vname c_tag i_str
      | FunctionEntry f -> Printf.sprintf "fun%d%s(%d)[%s]" f.svar.vid f.svar.vname c_tag i_str

    (* TODO: less hacky way (without ask_indices) to move node *)
    let is_live (n, c, i) = not (Spec.D.is_bot (get (n, c)))
    let move_opt (n, c, i) to_n =
      match ask_indices (to_n, c) with
      | [] -> None
      | [to_i] ->
        let to_node = (to_n, c, to_i) in
        BatOption.filter is_live (Some to_node)
      | _ :: _ :: _ ->
        failwith "Node.move_opt: ambiguous moved index"
    let equal_node_context (n1, c1, i1) (n2, c2, i2) =
      EQSys.LVar.equal (n1, c1) (n2, c2)
  end

  module NHT = BatHashtbl.Make (Node)

  let create entrystates: (module BiArg with type Node.t = MyCFG.node * Spec.C.t * int) =
    let (witness_prev_map, witness_prev, witness_next) =
      (* Get all existing vars *)
      let vars = NHT.create 100 in
      LHT.iter (fun lvar local ->
          ask_local lvar ~local (IterVars (fun i ->
              let lvar' = (fst lvar, snd lvar, i) in
              NHT.replace vars lvar' ()
            ))
        ) lh;

      let prev = NHT.create 100 in
      let next = NHT.create 100 in
      LHT.iter (fun lvar local ->
          ignore (ask_local lvar ~local (Queries.IterPrevVars (fun i (prev_node, prev_c_obj, j) edge ->
              let prev_lvar: NHT.key = (prev_node, Obj.obj prev_c_obj, j) in
              (* Exclude accumulated prevs, which were pruned *)
              if NHT.mem vars prev_lvar then (
                let lvar' = (fst lvar, snd lvar, i) in
                if M.tracing then M.trace "witness" "%s -( %a )-> %s\n" (Node.to_string prev_lvar) MyARG.pretty_inline_edge edge (Node.to_string lvar');
                NHT.modify_def [] lvar' (fun prevs -> (edge, prev_lvar) :: prevs) prev;
                NHT.modify_def [] prev_lvar (fun nexts -> (edge, lvar') :: nexts) next
              )
            )))
        ) lh;

      (prev,
       (fun n ->
          NHT.find_default prev n []), (* main entry is not in prev at all *)
       (fun n ->
          NHT.find_default next n [])) (* main return is not in next at all *)
    in
    let witness_main =
      let lvar = WitnessUtil.find_main_entry entrystates in
      let main_indices = ask_indices lvar in
      (* TODO: get rid of this hack for getting index of entry state *)
      assert (List.compare_length_with main_indices 1 = 0);
      let main_index = List.hd main_indices in
      (fst lvar, snd lvar, main_index)
    in

    let module Arg =
    struct
      module Node = Node
      module Edge = MyARG.InlineEdge
      let main_entry = witness_main
      let next = witness_next
    end
    in
    let module Arg =
    struct
      open MyARG
      module ArgIntra = UnCilTernaryIntra (UnCilLogicIntra (CfgIntra (FileCfg.Cfg)))
      include Intra (ArgIntra) (Arg)

      let prev = witness_prev
      let iter_nodes f =
        f main_entry;
        NHT.iter (fun n _ ->
            f n
          ) witness_prev_map

      let query ((n, c, i): Node.t) q =
        R.ask_local (n, c) (PathQuery (i, q))
    end
    in
    (module Arg: BiArg with type Node.t = MyCFG.node * Spec.C.t * int)

  let create entrystates =
    Timing.wrap "arg create" create entrystates
end
