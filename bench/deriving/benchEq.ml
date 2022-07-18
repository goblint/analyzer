(* dune exec bench/deriving/benchEq.exe -- -a *)

open Benchmark
open Benchmark.Tree


let () =
  let equal_manual_primitive ((x1: int), (y1: string)) ((x2: int), (y2: string)) = x1 = x2 && y1 = y2 in (* manual type annotations to be monomorphic like deriving *)
  let equal_deriving_primitive = [%eq: int * string] in

  let equal_manual_module ((x1: int), (y1: string)) ((x2: int), (y2: string)) = Int.equal x1 x2 && String.equal y1 y2 in
  let equal_deriving_module = [%eq: Int.t * String.t] in
  let equal_deriving_module_expand =
    let __1 () = String.equal
    and __0 () = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun (lhs0, lhs1) ->
        fun (rhs0, rhs1) ->
          ((fun x -> (__0 ()) x) lhs0 rhs0) &&
            ((fun x -> (__1 ()) x) lhs1 rhs1))
    [@ocaml.warning "-A"])
  in
  let equal_deriving_module_expand_simpl1 =
    let __1 () = String.equal
    and __0 () = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun (lhs0, lhs1) ->
        fun (rhs0, rhs1) ->
          ((__0 ()) lhs0 rhs0) && (* removed eta expansion: https://github.com/ocaml-ppx/ppx_deriving/pull/55 *)
            ((__1 ()) lhs1 rhs1))
    [@ocaml.warning "-A"])
  in
  let equal_deriving_module_expand_simpl2 =
    let __1 = String.equal
    and __0 = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun (lhs0, lhs1) ->
        fun (rhs0, rhs1) ->
          (__0 lhs0 rhs0) && (* removed quote () arg: https://github.com/ocaml-ppx/ppx_deriving/issues/57 *)
            (__1 lhs1 rhs1))
    [@ocaml.warning "-A"])
  in

  register (
    "pair" @>>> [
        "snd" @> lazy (
            let args = ((1, "foo"), (1, "bar")) in
            throughputN 1 [
              ("manual_primitive", Batteries.uncurry equal_manual_primitive, args);
              ("deriving_primitive", Batteries.uncurry equal_deriving_primitive, args);
              ("manual_module", Batteries.uncurry equal_manual_module, args);
              ("deriving_module", Batteries.uncurry equal_deriving_module, args);
              ("deriving_module_expand", Batteries.uncurry equal_deriving_module_expand, args);
              ("deriving_module_expand_simpl1", Batteries.uncurry equal_deriving_module_expand_simpl1, args);
              ("deriving_module_expand_simpl2", Batteries.uncurry equal_deriving_module_expand_simpl2, args);
            ]
          );
        "fst" @> lazy (
            let args = ((1, "foo"), (2, "bar")) in
            throughputN 1 [
              ("manual_primitive", Batteries.uncurry equal_manual_primitive, args);
              ("deriving_primitive", Batteries.uncurry equal_deriving_primitive, args);
              ("manual_module", Batteries.uncurry equal_manual_module, args);
              ("deriving_module", Batteries.uncurry equal_deriving_module, args);
              ("deriving_module_expand", Batteries.uncurry equal_deriving_module_expand, args);
              ("deriving_module_expand_simpl1", Batteries.uncurry equal_deriving_module_expand_simpl1, args);
              ("deriving_module_expand_simpl2", Batteries.uncurry equal_deriving_module_expand_simpl2, args);
            ]
          );
      ]
  )


let () =
  let equal_manual x1 x2 = Int.equal x1 x2 in
  let equal_deriving = [%eq: Int.t] in
  let equal_deriving_expand =
    let __0 () = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
        fun x -> (__0 ()) x)
      [@ocaml.warning "-A"])
  in
  let equal_deriving_expand_simpl1 =
    let __0 () = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
        __0 ()) (* removed eta expansion: https://github.com/ocaml-ppx/ppx_deriving/pull/55 *)
      [@ocaml.warning "-A"])
  in
  let equal_deriving_expand_simpl2 =
    let __0 = Int.equal in
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
        __0) (* removed quote () arg: https://github.com/ocaml-ppx/ppx_deriving/issues/57 *)
      [@ocaml.warning "-A"])
  in
  let equal_deriving_expand_simpl3 =
    ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
        Int.equal) (* inlined quote: https://github.com/ocaml-ppx/ppx_deriving/issues/57 *)
      [@ocaml.warning "-A"])
  in

  register (
    "module" @> lazy (
        let args = (1, 2) in
        throughputN 1 [
          ("manual", Batteries.uncurry equal_manual, args);
          ("deriving", Batteries.uncurry equal_deriving, args);
          ("deriving_expand", Batteries.uncurry equal_deriving_expand, args);
          ("deriving_expand_simpl1", Batteries.uncurry equal_deriving_expand_simpl1, args);
          ("deriving_expand_simpl2", Batteries.uncurry equal_deriving_expand_simpl2, args);
          ("deriving_expand_simpl3", Batteries.uncurry equal_deriving_expand_simpl3, args);
        ]
      )
  )

let () =
  run_global ()
