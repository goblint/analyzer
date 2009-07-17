open OUnit
open Pretty
open Cil

(* variables are strings *)
module StringVar =
struct
  type t = string
  let equal a b = a = b
  let pretty_trace () x = text x
  let hash = Hashtbl.hash 
end

(* domain is (reversed) integers *)
module Int  = IntDomain.Trier
module IntR = Lattice.Reverse(Int)

(* globals are varinfo -> IntR *)
module WhatGlob = Global.Make (IntR)

(* solver for that *)
module Solver = EffectWCon.Make (StringVar) (IntR) (WhatGlob)

(* generate a global *)
let g: Solver.global = makeGlobalVar "g" (TInt (IInt, []))

(* 
  1. x := g
  2. y := 8
  3. z := y
  4. z := z + 1 [also g := 42]
   *)
let system1 (v: Solver.lhs): Solver.rhs list = 
  match v with
    | "x" -> [(fun (loc, glob) -> (                  glob g                           ,[]                  ,[]))]
    | "y" -> [(fun (loc, glob) -> (ignore (loc "x"); Int.of_int 8L                    ,[]                  ,[]))]
    | "z" -> [(fun (loc, glob) -> (ignore (loc "y"); loc "y"                          ,[]                  ,[]))]
    | "w" -> [(fun (loc, glob) -> (ignore (loc "z"); Int.add (loc "w") (Int.of_int 1L),[(g,Int.of_int 42L)],[]))]
    | _   -> []

let test1 () = 
  let id x = x in
  let sol, gsol = Solver.solve system1 ["w"] in
  assert_equal ~printer:id "42" (Int.short 80 (Solver.GMap.find gsol g));
  assert_equal ~printer:id "42" (Int.short 80 (Solver.VMap.find sol "x"));
  assert_equal ~printer:id "8"  (Int.short 80 (Solver.VMap.find sol "y"));
  assert_equal ~printer:id "8"  (Int.short 80 (Solver.VMap.find sol "z"));
  assert_equal ~printer:id "Unknown int" (Int.short 80 (Solver.VMap.find sol "w"))

let test () = "solverTest" >:::
  [ "system1" >:: test1 ]
  
