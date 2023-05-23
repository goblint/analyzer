(** Finite automaton for matching an infeasible ARG path. *)

module type S =
sig
  type q
  type t

  val initial: q
  val next: q -> t -> q
  val accepting: q -> bool
end


module type KMPArg =
sig
  type t
  val equal: t -> t -> bool

  val pattern: t array
end

module KMP (KMPArg: KMPArg): S with type q = int and type t = KMPArg.t =
struct
  include KMPArg

  type q = int

  let initial = 0
  let m = Array.length pattern
  let accepting q = q = m

  (* let next_inner prefix q x =
    let q' = ref q in
    while !q' > 0 && not (equal pattern.(!q') x) do
      q' := prefix.(!q' - 1)
    done;
    if equal pattern.(!q') x then begin
      q' := !q' + 1
    end;
    !q' *)
  let rec next_inner prefix q x =
    if q > 0 && not (equal pattern.(q) x) then
      next_inner prefix prefix.(q - 1) x
    else if equal pattern.(q) x then
      q + 1
    else
      q

  (* CLRS *)
  let prefix: int array =
    let pi = Array.make m 0 in
    let k = ref 0 in
    for q = 2 to m do
      k := next_inner pi !k pattern.(q - 1);
      pi.(q - 1) <- !k
    done;
    pi

  let next (q: int) (x: t): int =
    if q = m then
      m
    else
      next_inner prefix q x
end
