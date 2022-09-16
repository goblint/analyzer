(** Local fixpoint iteration solvers that don't use a constraint system. *)

module Make (D: Lattice.S) =
struct
  let lfp ?(init=D.bot ()) (f: D.t -> D.t): D.t =
    let rec widening x =
      let x' = f x in
      let x'' = D.widen x (D.join x x') in
      if D.equal x x'' then
        narrowing_reuse x x' (* switch to narrowing phase *)
      else
        widening x''
    and narrowing_reuse x x' =
      let x'' = D.narrow x x' in
      if D.equal x x'' then
        x (* end iteration *)
      else
        narrowing x''
    and narrowing x =
      let x' = f x in
      narrowing_reuse x x'
    in
    widening init
end
