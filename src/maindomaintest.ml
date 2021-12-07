open! Defaults (* Enums / ... need initialized conf *)

module type FiniteSetElems =
sig
  type t
  val elems: t list
end

module FiniteSet (E:Printable.S) (Elems:FiniteSetElems with type t = E.t) =
struct
  module E =
  struct
    include E
    let arbitrary () = QCheck.oneofl Elems.elems
  end

  include SetDomain.Make (E)
  let top () = of_list Elems.elems
  let is_top x = equal x (top ())
end

module PrintableChar =
struct
  type t = char [@@deriving eq, ord, to_yojson]
  let name () = "char"
  let show x = String.make 1 x

  module P =
  struct
    type nonrec t = t
    let show = show
  end
  include Printable.Std
  include Printable.PrintSimple (P)

  let hash = Char.code

  let arbitrary () = QCheck.oneofl ['a'; 'b'; 'c'; 'd']
end

module ArbitraryLattice = FiniteSet (PrintableChar) (
  struct
    type t = char
    let elems = ['a'; 'b'; 'c'; 'd']
  end
)

module HoareArbitrary = HoareDomain.Set_LiftTop (ArbitraryLattice) (struct let topname = "Top" end)
module HoareArbitrary_NoTop = HoareDomain.Set (ArbitraryLattice)


module History (Base: Printable.S): Lattice.PO =
struct
  module P =
  struct
    include Printable.Liszt (Base)
    (* Prefix is stored in reversed order (main is last) since prepending is more efficient. *)
    let name () = "prefix"
  end
  module S =
  struct
    include SetDomain.Make (Base)
    let name () = "set"
  end
  include Printable.Prod (P) (S)

  let is_unique (_, s) =
    S.is_empty s

  let is_must_parent (p,s) (p',s') =
    if not (S.is_empty s) then
      false
    else
      let cdef_ancestor = P.common_suffix p p' in
      P.equal p cdef_ancestor

  let may_create (p,s) (p',s') =
    S.subset (S.union (S.of_list p) s) (S.union (S.of_list p') s')

  let compose ((p, s) as current) n =
    if BatList.mem_cmp Base.compare n p then (
      (* TODO: can be optimized by implementing some kind of partition_while function *)
      let s' = S.of_list (BatList.take_while (fun m -> not (Base.equal n m)) p) in
      let p' = List.tl (BatList.drop_while (fun m -> not (Base.equal n m)) p) in
      (p', S.add n (S.union s s'))
    )
    else if is_unique current then
      (n :: p, s)
    else
      (p, S.add n s)

  let leq (xp, xs) (yp, ys) =
    let p = P.common_prefix xp yp in
    if P.equal p yp then (
      let xp' = BatList.drop (List.length p) xp in
      S.equal S.(union (of_list xp') xs) ys
    )
    else
      false
  let join x y =
    if leq x y then
      y
    else if leq y x then
      x
    else
      failwith "join"
  let widen = join
  let meet x y =
    if leq x y then
      x
    else if leq y x then
      y
    else
      failwith "meet"
  let narrow = meet
  let pretty_diff () (x, y) = failwith "pretty_diff"
end

module HoareHistory = HoareDomain.Set_LiftTop (Lattice.LiftPO (History (PrintableChar)) (Printable.DefaultNames)) (struct let topname = "TOP" end)

let domains: (module Lattice.S) list = [
  (* (module IntDomainProperties.IntegerSet); (* TODO: top properties error *) *)
  (* (module IntDomain.Lifted); (* not abstraction of IntegerSet *) *)

  (* TODO: move to intDomains if passing *)
  (* (module IntDomain.Booleans); *)

  (* TODO: fix *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)

  (* (module ArbitraryLattice); *)
  (* (module HoareArbitrary); *)
  (* (module HoareArbitrary_NoTop); *)
  (* (module HoareDomain.MapBot (ArbitraryLattice) (HoareArbitrary)); *)
  (* (module HoareDomain.MapBot (ArbitraryLattice) (HoareArbitrary_NoTop)); *)

  (module HoareHistory);
]

let nonAssocDomains: (module Lattice.S) list = []

let intDomains: (module IntDomainProperties.S) list = [
  (* (module IntDomain.Interval); *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.Congruence); *)
  (* (module IntDomain.Flattened); *)
  (* (module IntDomain.Interval32); *)
  (* (module IntDomain.Booleans); *)
  (* (module IntDomain.IntDomTuple); *)
]

let nonAssocIntDomains: (module IntDomainProperties.S) list = [
  (* (module IntDomain.DefExc); *)
]

(* TODO: make arbitrary ikind part of domain test for better efficiency *)
let ikinds: Cil.ikind list = [
  (* TODO: enable more, some seem to break things *)
  IChar;
  ISChar;
  (* IUChar; *)
  (* IBool; *)
  IInt;
  (* IUInt; *)
  IShort;
  (* IUShort; *)
  (* ILong; *)
  (* IULong; *)
  (* ILongLong; *)
  (* IULongLong; *)
]

let testsuite =
  domains
  |> List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.All (D) in
      DP.tests
    )
  |> List.flatten
let nonAssocTestsuite =
  nonAssocDomains
  |> List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.AllNonAssoc (D) in
      DP.tests
    )
  |> List.flatten

let old_intdomains intDomains =
  BatList.cartesian_product intDomains ikinds
  |> List.map (fun (d, ik) ->
      let module D = (val d: IntDomainProperties.S) in
      let module Ikind = struct let ikind () = ik end in
      (module IntDomainProperties.WithIkind (D) (Ikind): IntDomainProperties.OldS)
    )
let intTestsuite =
  old_intdomains intDomains
  |> List.map (fun d ->
      let module D = (val d: IntDomainProperties.OldS) in
      let module DP = IntDomainProperties.All (D) in
      DP.tests
    )
  |> List.flatten
let nonAssocIntTestsuite =
  old_intdomains nonAssocIntDomains
  |> List.map (fun d ->
      let module D = (val d: IntDomainProperties.OldS) in
      let module DP = IntDomainProperties.AllNonAssoc (D) in
      DP.tests
    )
  |> List.flatten
let () =
  QCheck_base_runner.run_tests_main ~argv:Sys.argv (testsuite @ nonAssocTestsuite @ intTestsuite @ nonAssocIntTestsuite)
