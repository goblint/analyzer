(** Our Control-flow graph implementation. *)

open Cil

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
  | ASM of string list * (Edge.asm_out * Edge.asm_in * string list) option
  | VDecl of CilType.Varinfo.t
  | Skip
  | SelfLoop


type edges = (location * edge) list

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


module NodeH = BatHashtbl.Make (Node)


let current_node : node option ref = ref None

let unknown_exp : exp = mkString "__unknown_value__"
let dummy_func = emptyFunction "__goblint_dummy_init" (* TODO get rid of this? *)
let dummy_node = FunctionEntry Cil.dummyFunDec

let all_array_index_exp : exp = CastE(TInt(Cilfacade.ptrdiff_ikind (),[]), unknown_exp)
