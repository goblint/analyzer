open Goblint_lib
open OUnit2
open GoblintCil
open Pretty
open ConstrSys
open Goblint_solver

(* variables are strings *)
module StringVar =
struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let pretty_trace () x = text x
  let compare = compare
  let hash (x:t) = Hashtbl.hash x
  let printXml _ _ = ()
  let var_id x = x
  let node _ = failwith "no node"
  let relift x = x
  let is_write_only _ = false
end

(* domain is (reversed) integers *)
module Ikind = struct let ikind () = Cil.ILong end
module Int  = IntDomainProperties.WithIkind (IntDomain.DefExc) (Ikind)
module IntR = Lattice.Reverse(Int)

module ConstrSys = struct
  module LVar = StringVar
  module GVar = StringVar
  module D = Int
  module G = IntR

  (*
    1. x := g
    2. y := 8
    3. z := y
    4. w := w + 1 [also g := 42; _ := z]
    *)
  let system : LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) option = function
    | "x" -> Some (fun loc _ glob gside -> glob "g")
    | "y" -> Some (fun loc _ glob gside -> (ignore (loc "x"); Int.of_int (Z.of_int64 8L)))
    | "z" -> Some (fun loc _ glob gside -> (ignore (loc "y"); loc "y"))
    | "w" -> Some (fun loc _ glob gside -> (gside "g" (Int.of_int (Z.of_int64 42L)); ignore (loc "z"); try Int.add (loc "w") (Int.of_int (Z.of_int64 1L)) with IntDomain.ArithmeticOnIntegerBot _ -> Int.top ()))
    | _   -> None

  let iter_vars _ _ _ _ _ = ()
  let sys_change _ _ = {obsolete = []; delete = []; reluctant = []; restart = []}
  let postmortem _ = []
end

module LH = BatHashtbl.Make (ConstrSys.LVar)
module GH = BatHashtbl.Make (ConstrSys.GVar)
module PostSolverArg =
struct
  let should_prune = false
  let should_verify = false
  let should_warn = false
  let should_save_run = false
end
module Solver = GlobSolverFromEqSolver (PostSolver.EqIncrSolverFromEqSolver (EffectWConEq.Make) (PostSolverArg)) (ConstrSys) (LH) (GH)

let test1 _ =
  let id x = x in
  let ((sol, gsol), _) = Solver.solve [] [] ["w"] None in
  assert_equal ~printer:id "42" (Int.show (GH.find gsol "g"));
  assert_equal ~printer:id "42" (Int.show (LH.find sol "x"));
  assert_equal ~printer:id "8"  (Int.show (LH.find sol "y"));
  assert_equal ~printer:id "8"  (Int.show (LH.find sol "z"));
  assert_equal ~printer:id "Unknown int([-63,63])" (Int.show (LH.find sol "w"))

let test () =
  "solverTest" >::: [
    "system1" >:: test1
  ]
