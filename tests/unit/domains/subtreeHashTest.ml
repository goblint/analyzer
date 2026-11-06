open Goblint_lib
open OUnit2

module Key = struct
  include Printable.Strings
  let tag = int_of_string
end

let value_hash_calls = ref 0
module Value = struct
  include Lattice.Fake (Printable.Strings)
  let hash v = incr value_hash_calls; Hashtbl.hash v
end
module Reference = MapDomain.PatriciaMapBot (Key) (Value)
module Cached = MapDomain.SubtreeHashCachedPatriciaMapBot (Key) (Value)

let check reference cached =
  assert_equal (Reference.bindings reference) (Cached.bindings cached);
  assert_equal ~printer:string_of_int (Reference.hash reference) (Cached.hash cached)

let test_operations _ =
  let rng = Random.State.make [|42|] in
  let reference = ref (Reference.empty ()) and cached = ref (Cached.empty ()) in
  let saved = ref [] in
  for i = 0 to 499 do
    let key = string_of_int (Random.State.int rng 128 - 64) in
    let value = string_of_int i in
    if i mod 5 = 0 then (
      reference := Reference.remove key !reference;
      cached := Cached.remove key !cached
    ) else (
      reference := Reference.add key value !reference;
      cached := Cached.add key value !cached
    );
    check !reference !cached;
    if i mod 50 = 0 then saved := (!reference, !cached) :: !saved
  done;
  List.iter (fun (r, c) ->
      check r c;
      check (Reference.map ((^) "x") r) (Cached.map ((^) "x") c);
      let update k v = if int_of_string k mod 2 = 0 then v else v ^ "!" in
      check (Reference.mapi update r) (Cached.mapi update c);
      let keep k _ = int_of_string k mod 3 = 0 in
      check (Reference.filter keep r) (Cached.filter keep c);
      let combine a b = if String.compare a b > 0 then a else b in
      check (Reference.idempotent_union combine r !reference) (Cached.idempotent_union combine c !cached);
      check (Reference.nonidempotent_inter ((^) ) r !reference) (Cached.nonidempotent_inter ((^) ) c !cached);
      let diff a b = if a = b then None else Some (a ^ b) in
      check (Reference.difference diff r !reference) (Cached.difference diff c !cached);
      check (Reference.relift r) (Cached.relift c);
      let restored = Marshal.from_string (Marshal.to_string c []) 0 in
      check r restored;
      let reversed = Cached.add_list (List.rev (Cached.bindings c)) (Cached.empty ()) in
      assert_bool "equal maps built in reverse order" (Cached.equal c reversed);
      assert_equal (Cached.hash c) (Cached.hash reversed)
    ) !saved;
  let extremes = List.map (fun k -> string_of_int k, "value") [min_int; max_int; -1; 0; 1] in
  check (Reference.add_list extremes !reference) (Cached.add_list extremes !cached)

let test_reuse _ =
  let bindings = List.init 2048 (fun i -> string_of_int i, string_of_int i) in
  let original = Cached.add_list bindings (Cached.empty ()) in
  value_hash_calls := 0;
  let h = Cached.hash original in
  assert_equal 2048 !value_hash_calls;
  value_hash_calls := 0;
  assert_equal h (Cached.hash original);
  assert_equal 0 !value_hash_calls;
  let updated = Cached.add "512" "changed" original in
  assert_equal 0 !value_hash_calls;
  ignore (Cached.hash updated);
  assert_equal ~msg:"only the changed binding is rehashed" 1 !value_hash_calls;
  value_hash_calls := 0;
  ignore (Cached.hash (Cached.remove "513" updated));
  assert_equal ~msg:"deletion reuses hashes of surviving bindings" 0 !value_hash_calls;
  assert_equal h (Cached.hash original);
  assert_equal "512" (Cached.find "512" original)

let tests = "subtree_hash" >::: [
    "operations" >:: test_operations;
    "reuse" >:: test_reuse;
  ]
