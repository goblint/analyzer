(** Specification for global solver variables. *)

module type S =
sig
  module Val: Lattice.S 
  module Var :
  sig
    include Hashtbl.HashedType
    val pretty_trace: unit -> t -> Pretty.doc
    val compare : t -> t -> int
  end with type t = Basetype.Variables.t

end


module Make (G: Lattice.S) = 
struct 
  module Val = G
  module Var = Basetype.Variables
end

module Prod (G1: S) (G2: S with module Var = G1.Var) =
struct
  module Val = Lattice.Prod (G1.Val) (G2.Val)
  module Var = G1.Var

  (* Function that merges the side_effects by pairing them with the bottom value
   * of the other domain. *)
  let merge_effects gd1 gd2 = 
    let gd1 = List.map (fun (g,x) -> (g,(x,G2.Val.bot ()))) gd1 in
    let gd2 = List.map (fun (g,x) -> (g,(G1.Val.bot (), x))) gd2 in
      gd1 @ gd2

  let split_effects gd =
    let gs, ds = List.split gd in
    let d1s,d2s = List.split ds in
      List.combine gs d1s, List.combine gs d2s
end
