open OUnit
open Pretty
open Cil

(* variables are strings *)
module StringVar =
struct
  type t = string
  let equal (a:t) (b:t) = a = b
  let pretty_trace () x = text x
  let compare = compare
  let hash (x:t) = Hashtbl.hash x
  let category _ = 1
  let printXml _ _ = ()
  let var_id x = x
  let file_name x = x
  let line_nr _ = 1
  let node _ = failwith "no node"
end

(* domain is (reversed) integers *)
module Int  = IntDomain.Trier
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
    4. z := z + 1 [also g := 42]
    *)
  let system : LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) list = function
    | "x" -> [fun loc _ glob gside -> glob "g"]
    | "y" -> [fun loc _ glob gside -> (ignore (loc "x"); Int.of_int 8L)]
    | "z" -> [fun loc _ glob gside -> (ignore (loc "y"); loc "y")]
    | "w" -> [fun loc _ glob gside -> (gside "g" (Int.of_int 42L); ignore (loc "z"); Int.add (loc "w") (Int.of_int 1L))]
    | _   -> []
end

module LH = BatHashtbl.Make (ConstrSys.LVar)
module GH = BatHashtbl.Make (ConstrSys.GVar)
module Solver = EffectWCon.Make2 (ConstrSys) (LH) (GH)

let test1 () = 
  let id x = x in
  let sol, gsol = Solver.solve [] [] ["w"] in
  assert_equal ~printer:id "42" (Int.short 80 (GH.find gsol "g"));
  assert_equal ~printer:id "42" (Int.short 80 (LH.find sol "x"));
  assert_equal ~printer:id "8"  (Int.short 80 (LH.find sol "y"));
  assert_equal ~printer:id "8"  (Int.short 80 (LH.find sol "z"));
  assert_equal ~printer:id "Unknown int" (Int.short 80 (LH.find sol "w"))

let test () = "solverTest" >:::
  [ "system1" >:: test1 ]