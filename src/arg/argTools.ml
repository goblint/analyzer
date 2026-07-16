(** Construction of {{!MyARG} ARGs} from constraint system solutions. *)

open MyCFG

module M = Messages

module type BiArg =
sig
  include MyARG.S with module Edge = MyARG.InlineEdge

  val prev: Node.t -> (Edge.t * Node.t) list
  val iter_nodes: (Node.t -> unit) -> unit

  val query: Node.t -> 'a Queries.t -> 'a Queries.result
end

module Stack (Arg: BiArg): BiArg with module Node = MyARG.StackNode (Arg.Node) =
struct
  include MyARG.Stack (Arg)

  let prev _ = failwith "TODO"

  module NHT = BatHashtbl.Make (Node)

  (** Iterate over {e reachable} nodes. *)
  let iter_nodes (f: Node.t -> unit): unit = (* Copied from Make.create below. *)
    let reachable = NHT.create 100 in

    (* DFS *)
    let rec iter_node node =
      if not (NHT.mem reachable node) then (
        NHT.replace reachable node ();
        f node;
        List.iter (fun (edge, to_node) ->
            iter_node to_node
          ) (next node)
      )
    in

    iter_node main_entry

  let query nl = Arg.query (List.hd nl)
end

module type NodeStyles =
sig
  type node
  val extra_node_styles: node -> string list
end

module EnumerateNode (Node: MyARG.Node): MyARG.Node with type t = Node.t =
struct
  include Node

  module NH = Hashtbl.Make (Node)
  let node_numbers: int NH.t = NH.create 100
  let next_number = ref 0

  let to_string node =
    let number = match NH.find_opt node_numbers node with
      | Some number -> number
      | None ->
        let number = !next_number in
        NH.replace node_numbers node number;
        next_number := number + 1;
        number
    in
    "N" ^ string_of_int number
end

module Dot (Arg: BiArg) (NodeStyles: NodeStyles with type node = Arg.Node.t) =
struct
  let dot_node_name ppf node =
    Format.fprintf ppf "\"%s\"" (Arg.Node.to_string node)

  let dot_edge ppf from_node (edge, to_node) =
    let label = [Format.sprintf "label=\"%s\"" (String.escaped (Arg.Edge.to_string edge))] in
    let style = match edge with
      | MyARG.InlinedEdge _ -> ["style=dotted"]
      | _ -> []
    in
    let styles = String.concat "," (label @ style) in
    Format.fprintf ppf "@,%a -> %a [%s];" dot_node_name from_node dot_node_name to_node styles

  let dot_node ppf node =
    let shape = match Arg.Node.cfgnode node with
      | Statement {skind=If (_,_,_,_,_); _}  -> ["shape=diamond"]
      | Statement _ -> [] (* use default shape *)
      | Function _
      | FunctionEntry _ -> ["shape=box"]
    in
    let styles = String.concat "," (shape @ NodeStyles.extra_node_styles node) in
    Format.fprintf ppf "@,%a [%s];" dot_node_name node styles;
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
    type t = Node.t * Spec.C.t * int [@@deriving eq, ord, hash]

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
  end

  module NHT = BatHashtbl.Make (Node)

  let create entrystates: (module BiArg) =
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
                if M.tracing then M.trace "witness" "%s -( %a )-> %s" (Node.to_string prev_lvar) MyARG.pretty_inline_edge edge (Node.to_string lvar');
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
      module Node: MyARG.Node with type t = Node.t =
        (val match GobConfig.get_string "exp.arg.id" with
           | "node" ->
             (module Node: MyARG.Node with type t = Node.t)
           | "enumerate" ->
             (module EnumerateNode (Node): MyARG.Node with type t = Node.t)
           | _ -> failwith "exp.arg.id: illegal value"
        )

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

      (** Iterate over {e reachable} nodes. *)
      let iter_nodes (f: Node.t -> unit): unit =
        let reachable = NHT.create (NHT.length witness_prev_map) in

        (* DFS *)
        let rec iter_node node =
          if not (NHT.mem reachable node) then (
            NHT.replace reachable node ();
            f node;
            List.iter (fun (edge, to_node) ->
                iter_node to_node
              ) (next node) (* use included next, not Arg.next, to prune uncilled nodes *)
          )
        in

        iter_node main_entry

      let query ((n, c, i): Node.t) q =
        R.ask_local (n, c) (PathQuery (i, q))
    end
    in
    let module Arg =
      (val if GobConfig.get_bool "exp.arg.stack" then
          (module Stack (Arg): BiArg)
        else
          (module Arg: BiArg)
      )
    in
    (module Arg: BiArg)

  let create entrystates =
    Timing.wrap "arg create" create entrystates
end
