(** Control-flow graph.

    Distinct from CIL's CFG. *)

open GoblintCil

(** Re-exported [Node.t] with constructors. See [Node.t] for documentation. *)
type node = Node.t =
  | Statement of CilType.Stmt.t
  | FunctionEntry of CilType.Fundec.t
  | Function of CilType.Fundec.t

(** Re-exported [Edge.t] with constructors. See [Edge.t] for documentation. *)
type edge = Edge.t =
  | Assign of CilType.Lval.t * CilType.Exp.t
  | Proc of CilType.Lval.t option * CilType.Exp.t * CilType.Exp.t list
  | Entry of CilType.Fundec.t
  | Ret of CilType.Exp.t option * CilType.Fundec.t
  | Test of CilType.Exp.t * bool
  | ASM of string list * Edge.asm_out * Edge.asm_in
  | VDecl of CilType.Varinfo.t
  | Skip


type edges = (CilType.Location.t * Edge.t) list [@@deriving eq, hash]

type cfg = node -> (edges * node) list

module type CfgBackward =
sig
  val prev: cfg
end

module type CfgForward =
sig
  val next: cfg
end

module type CfgBidir =
sig
  include CfgBackward
  include CfgForward
end

(** Type of CFG "edges": keyed by 'from' and 'to' nodes,
    along with the list of connecting instructions. *)
module CfgEdge = struct
  type t = Node.t * edges * Node.t [@@deriving eq, hash]
end

module CfgEdgeH = BatHashtbl.Make (CfgEdge)

module type CfgBidirSkip =
sig
  include CfgBidir
  val skippedByEdge: node -> edges -> node -> stmt list
  (** [skippedByEdge from edges to] returns the list of {{!GoblintCil.stmt} AST statements} skipped over by [find_real_stmt] in {!CfgTools.createCfg}.
      This consists of statements which do not correspond to CFG nodes, but some surrounding AST constructions. *)
end


module NodeH = BatHashtbl.Make (Node)


let current_node = Node.current_node
let current_cfg : (module CfgBidirSkip) ref =
  let module Cfg =
  struct
    let next _ = raise Not_found
    let prev _ = raise Not_found
    let skippedByEdge _ _ _ = raise Not_found
  end
  in
  ref (module Cfg: CfgBidirSkip)

let unknown_exp : exp = mkString "__unknown_value__"
let dummy_func = emptyFunction "__goblint_dummy_init" (* TODO get rid of this? *)
let dummy_node = FunctionEntry Cil.dummyFunDec


module type FileCfg =
sig
  val file: Cil.file
  module Cfg: CfgBidirSkip
end
