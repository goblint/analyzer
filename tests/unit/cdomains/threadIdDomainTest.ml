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

let test_history_must_be_ancestor _ =
  let open History in
  let assert_equal = assert_equal ~printer:string_of_bool in

  (* non-unique is not must parent *)
  assert_equal false (must_be_ancestor (main >> a >> a) (main >> a >> a));
  assert_equal false (must_be_ancestor (main >> a >> a) (main >> a >> a >> a));
  assert_equal false (must_be_ancestor (main >> a >> a) (main >> a >> a >> b));

  (* unique is not self-parent *)
  assert_equal false (must_be_ancestor main main);
  assert_equal false (must_be_ancestor (main >> a) (main >> a));
  assert_equal false (must_be_ancestor (main >> a >> b) (main >> a >> b));

  (* unique is must parent if prefix *)
  assert_equal true (must_be_ancestor main (main >> a));
  assert_equal true (must_be_ancestor main (main >> a >> a));
  assert_equal true (must_be_ancestor main (main >> a >> b));
  assert_equal true (must_be_ancestor (main >> a) (main >> a >> b));
  assert_equal false (must_be_ancestor (main >> a) main);
  assert_equal false (must_be_ancestor (main >> b) (main >> a >> b));
  assert_equal false (must_be_ancestor (main >> a) (main >> b >> a));
  assert_equal false (must_be_ancestor (main >> a) (main >> a >> a)); (* may be created by just main (non-uniquely) *)
  ()

let test_history_may_be_ancestor _ =
  let open History in
  let assert_equal = assert_equal ~printer:string_of_bool in

  (* unique may only be created by unique (prefix) *)
  assert_equal true (may_be_ancestor main (main >> a));
  assert_equal true (may_be_ancestor main (main >> a >> b));
  assert_equal true (may_be_ancestor (main >> a) (main >> a >> b));
  assert_equal false (may_be_ancestor (main >> a) (main >> a)); (* infeasible for race: definitely_not_started allows equality *)
  assert_equal false (may_be_ancestor (main >> b) (main >> a >> b)); (* 53-races-mhp/04-not-created2 *)
  assert_equal false (may_be_ancestor (main >> a >> a) (main >> a >> b)); (* infeasible for race: cannot create non-unique (main >> a >> a) before unique (main >> a >> b) *)

  (* unique creates non-unique and is prefix: added elements cannot be in prefix *)
  assert_equal true (may_be_ancestor main (main >> a >> a));
  assert_equal true (may_be_ancestor main (main >> a >> b >> b));
  assert_equal true (may_be_ancestor (main >> a) (main >> a >> b >> b));
  (* No false tests because added elements condition is always true by construction in unit test harness. *)

  (* non-unique created by unique and is prefix: removed elements must be in set *)
  assert_equal true (may_be_ancestor (main >> a) (main >> a >> a));
  assert_equal true (may_be_ancestor (main >> a >> b) (main >> a >> b >> b));
  assert_equal true (may_be_ancestor (main >> a >> b) (main >> a >> b >> a));
  assert_equal false (may_be_ancestor (main >> a >> b) (main >> a >> a)); (* infeasible for race: definitely_not_started requires (main >> a), where this must happen, to be must parent for (main >> a >> a), which it is not *)
  assert_equal false (may_be_ancestor (main >> a >> b) (main >> b >> b)); (* infeasible for race: definitely_not_started requires (main >> a), where this must happen, to be must parent for (main >> b >> b), which it is not *)

  (* unique creates non-unique and prefixes are incompatible *)
  assert_equal false (may_be_ancestor (main >> a) (main >> b >> a >> a)); (* 53-races-mhp/05-not-created3 *)
  assert_equal false (may_be_ancestor (main >> a >> b) (main >> b >> a >> c >> c)); (* infeasible for race: definitely_not_started requires (main >> a), where this must happen, to be must parent for (main >> b >> a >> c >> c), which it is not *)
  assert_equal false (may_be_ancestor (main >> a >> b) (main >> a >> c >> d >> d)); (* 53-races-mhp/06-not-created4, also passes with simple may_be_ancestor *)

  (* non-unique creates non-unique: prefix must not lengthen *)
  assert_equal false (may_be_ancestor (main >> a >> a) (main >> a >> b >> b)); (* infeasible for race: cannot create non-unique (main >> a >> a) before unique prefix-ed (main >> a >> b >> b) *)
  assert_equal false (may_be_ancestor (main >> a >> a) (main >> b >> a >> a)); (* 53-races-mhp/07-not-created5 *)
  (* non-unique creates non-unique: prefix must be compatible *)
  assert_equal false (may_be_ancestor (main >> a >> b >> c >> c) (main >> b >> a >> c >> c)); (* infeasible for race: definitely_not_started requires (main >> a >> b or main >> a >> b >> c), where this must happen, to be must parent for (main >> b >> a >> c >> c), which it is not *)
  (* non-unique creates non-unique: elements must not be removed *)
  assert_equal false (may_be_ancestor (main >> a >> b >> b) (main >> a >> c >> c)); (* from set *) (* 53-races-mhp/08-not-created6, also passes with simple may_be_ancestor *)
  assert_equal false (may_be_ancestor (main >> a >> b >> b) (main >> b >> b)); (* from prefix *) (* infeasible for race: definitely_not_started requires (main >> a or main >> a >> b), where this must happen, to be must parent for (main >> b >> b), which it is not *)
  (* non-unique creates non-unique: removed elements and set must be in new set *)
  assert_equal false (may_be_ancestor (main >> a >> b >> c >> c) (main >> a >> c >> c)); (* already fails previous condition *)
  (* No false tests because already fails previous by construction in unit test harness. *)
  (* non-unique creates non-unique *)
  assert_equal true (may_be_ancestor (main >> a >> a) (main >> a >> a));
  assert_equal true (may_be_ancestor (main >> a >> a) (main >> a >> a >> b));
  assert_equal true (may_be_ancestor (main >> a >> a) (main >> a >> b >> a));
  assert_equal true (may_be_ancestor (main >> a >> a) (main >> a >> b >> c >> a));
  assert_equal true (may_be_ancestor (main >> a >> b >> b) (main >> a >> b >> b));
  assert_equal true (may_be_ancestor (main >> a >> b >> b) (main >> a >> a >> b));
  assert_equal true (may_be_ancestor (main >> a >> b >> b) (main >> a >> b >> a));
  assert_equal true (may_be_ancestor (main >> a >> b >> b) (main >> b >> b >> a));
  assert_equal true (may_be_ancestor (main >> a >> b >> b) (main >> b >> a >> b));

  (* Some tests may still be missing because commit 4f6a7637b8d0dc723fe382f94bed6c822cd4a2ce passed all before two additional improvements.
     Might be related to untestability with this unit test harness: https://github.com/goblint/analyzer/pull/1561#discussion_r1888149978. *)
  ()

let test_history_must_ancestors _ =
  let open History in
  let compare_ancestors a1 a2 = Option.equal (List.equal equal) a1 a2 in
  let print_ancestors a =
    let string_of_thread t =
      GoblintCil.Pretty.sprint ~width:max_int (History.pretty () t)
    in
    match a with
    | Some l -> "Some [" ^ String.concat ", " (List.map string_of_thread l) ^ "]"
    | None -> "None"
  in

  let assert_equal =
    assert_equal ?printer:(Some print_ancestors) ?cmp:(Some compare_ancestors)
  in

  (* unique tids *)
  assert_equal (Some []) (must_ancestors main);
  assert_equal (Some [ main ]) (must_ancestors (main >> a));
  assert_equal (Some [ main; main >> a; main >> a >> b; main >> a >> b >> c ]) (must_ancestors (main >> a >> b >> c >> d));

  (* non-unique tids *)
  assert_equal (Some [ main ]) (must_ancestors (main >> a >> a));
  assert_equal (Some [ main ]) (must_ancestors (main >> a >> a >> b));
  assert_equal (Some [ main ]) (must_ancestors (main >> a >> b >> c >> a));
  assert_equal (Some [ main; main >> a ]) (must_ancestors (main >> a >> b >> c >> b >> d));
  assert_equal (Some [ main; main >> a; main >> a >> b ]) (must_ancestors (main >> a >> b >> c >> c));

  ()


let tests =
  "threadIdDomainTest" >::: [
    "history" >::: [
      "must_be_ancestor" >:: test_history_must_be_ancestor;
      "may_be_ancestor" >:: test_history_may_be_ancestor;
      "must_ancestors" >:: test_history_must_ancestors;
    ]
  ]
