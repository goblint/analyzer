open OUnit2
open Goblint_lib
open ThreadIdDomain

module History = History (FunNode)

let main = GoblintCil.makeGlobalVar "main" GoblintCil.voidType
let a = GoblintCil.makeGlobalVar "a" GoblintCil.voidType
let b = GoblintCil.makeGlobalVar "b" GoblintCil.voidType
let c = GoblintCil.makeGlobalVar "c" GoblintCil.voidType
let d = GoblintCil.makeGlobalVar "d" GoblintCil.voidType

let main: History.t = History.threadinit main ~multiple:false

let (>>) (parent: History.t) (v: GoblintCil.varinfo): History.t =
  match History.threadenter ~multiple:false (parent, History.D.bot ()) MyCFG.dummy_node None v with
  | [child] -> child
  | _ -> assert false

let test_history_is_must_parent _ =
  let open History in
  let assert_equal = assert_equal ~printer:string_of_bool in

  (* non-unique is not must parent *)
  assert_equal false (is_must_parent (main >> a >> a) (main >> a >> a));
  assert_equal false (is_must_parent (main >> a >> a) (main >> a >> a >> a));
  assert_equal false (is_must_parent (main >> a >> a) (main >> a >> a >> b));

  (* unique is not self-parent *)
  assert_equal false (is_must_parent main main);
  assert_equal false (is_must_parent (main >> a) (main >> a));
  assert_equal false (is_must_parent (main >> a >> b) (main >> a >> b));

  (* unique is must parent if prefix *)
  assert_equal true (is_must_parent main (main >> a));
  assert_equal true (is_must_parent main (main >> a >> a));
  assert_equal true (is_must_parent main (main >> a >> b));
  assert_equal true (is_must_parent (main >> a) (main >> a >> b));
  assert_equal false (is_must_parent (main >> a) main);
  assert_equal false (is_must_parent (main >> b) (main >> a >> b));
  assert_equal false (is_must_parent (main >> a) (main >> b >> a));
  assert_equal false (is_must_parent (main >> a) (main >> a >> a)); (* may be created by just main (non-uniquely) *)
  ()

let test_history_may_create _ =
  let open History in
  let assert_equal = assert_equal ~printer:string_of_bool in

  (* unique may only be created by unique (prefix) *)
  assert_equal true (may_create main (main >> a));
  assert_equal true (may_create main (main >> a >> b));
  assert_equal true (may_create (main >> a) (main >> a >> b));
  assert_equal false (may_create (main >> a) (main >> a));
  assert_equal false (may_create (main >> b) (main >> a >> b));
  assert_equal false (may_create (main >> a >> a) (main >> a >> b));

  (* unique creates non-unique and is prefix: added elements cannot be in prefix *)
  assert_equal true (may_create main (main >> a >> a));
  assert_equal true (may_create main (main >> a >> b >> b));
  assert_equal true (may_create (main >> a) (main >> a >> b >> b));
  (* TODO: added elements condition always true by construction in tests? *)

  (* non-unique created by unique and is prefix: removed elements must be in set *)
  assert_equal true (may_create (main >> a) (main >> a >> a));
  assert_equal true (may_create (main >> a >> b) (main >> a >> b >> b));
  assert_equal true (may_create (main >> a >> b) (main >> a >> b >> a));
  assert_equal false (may_create (main >> a >> b) (main >> a >> a));
  assert_equal false (may_create (main >> a >> b) (main >> b >> b));

  (* unique creates non-unique and prefixes are incompatible *)
  assert_equal false (may_create (main >> a) (main >> b >> a >> a));
  assert_equal false (may_create (main >> a >> b) (main >> b >> a >> c >> c));
  assert_equal false (may_create (main >> a >> b) (main >> a >> c >> d >> d));

  (* non-unique creates non-unique: prefix must not lengthen *)
  assert_equal false (may_create (main >> a >> a) (main >> a >> b >> b));
  assert_equal false (may_create (main >> a >> a) (main >> b >> a >> a));
  (* non-unique creates non-unique: prefix must be compatible *)
  assert_equal false (may_create (main >> a >> b >> c >> c) (main >> b >> a >> c >> c));
  (* non-unique creates non-unique: elements must not be removed *)
  assert_equal false (may_create (main >> a >> b >> b) (main >> a >> c >> c)); (* from set *)
  assert_equal false (may_create (main >> a >> b >> b) (main >> b >> b)); (* from prefix *)
  (* non-unique creates non-unique: removed elements and set must be in new set *)
  (* assert_equal false (may_create (main >> a >> b >> c >> c) (main >> a >> c >> c)); *)
  (* TODO: cannot test due because by construction after prefix check? *)
  (* non-unique creates non-unique *)
  assert_equal true (may_create (main >> a >> a) (main >> a >> a));
  assert_equal true (may_create (main >> a >> a) (main >> a >> a >> b));
  assert_equal true (may_create (main >> a >> a) (main >> a >> b >> a));
  assert_equal true (may_create (main >> a >> a) (main >> a >> b >> c >> a));
  assert_equal true (may_create (main >> a >> b >> b) (main >> a >> b >> b));
  assert_equal true (may_create (main >> a >> b >> b) (main >> a >> a >> b));
  assert_equal true (may_create (main >> a >> b >> b) (main >> a >> b >> a));
  assert_equal true (may_create (main >> a >> b >> b) (main >> b >> b >> a));
  assert_equal true (may_create (main >> a >> b >> b) (main >> b >> a >> b));

  (* 4f6a7637b8d0dc723fe382f94bed6c822cd4a2ce passes all... *)
  ()

let tests =
  "threadIdDomainTest" >::: [
    "history" >::: [
      "is_must_parent" >:: test_history_is_must_parent;
      "may_create" >:: test_history_may_create;
    ]
  ]
