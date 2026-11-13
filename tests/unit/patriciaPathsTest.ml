open Goblint_lib
open OUnit2

module Representative = struct
  include Printable.Strings
  type elt = t
  let of_elt x = x
  let hash x = String.length x mod 2
end
module B = SetDomain.Make (Printable.Strings)
module T = DisjointDomain.PatriciaProjectiveSet (Printable.Strings) (B) (Representative)
module O = DisjointDomain.ProjectiveSet (Printable.Strings) (B) (Representative)

let build add empty xs = List.fold_left (fun s x -> add x s) empty xs
let elements_t x = List.sort String.compare (T.elements x)
let elements_o x = List.sort String.compare (O.elements x)

let tests = "patricia_paths" >::: [
    "collision_operations" >:: (fun _ ->
        let samples = [ []; ["aa"]; ["aa"; "ab"]; ["a"; "b"; "abc"]; [""; "a"; "aa"; "ab"] ] in
        List.iter (fun xs ->
            let t = build T.add (T.empty ()) xs and o = build O.add (O.empty ()) xs in
            assert_equal (elements_o o) (elements_t t);
            assert_equal (O.cardinal o) (T.cardinal t);
            List.iter (fun x ->
                assert_equal (O.mem x o) (T.mem x t);
                assert_equal (elements_o (O.remove x o)) (elements_t (T.remove x t))
              ) [""; "a"; "aa"; "ab"; "missing"];
            List.iter (fun ys ->
                let u = build T.add (T.empty ()) ys and p = build O.add (O.empty ()) ys in
                assert_equal (O.leq o p) (T.leq t u);
                assert_equal (O.equal o p) (T.equal t u);
                assert_equal (elements_o (O.join o p)) (elements_t (T.join t u));
                assert_equal (elements_o (O.inter o p)) (elements_t (T.inter t u));
                assert_equal (elements_o (O.diff o p)) (elements_t (T.diff t u))
              ) samples
          ) samples);
    "relift_reindexes" >:: (fun _ ->
        let salt = ref 0 in
        let module R = struct
          include Representative
          let hash x = hash x + !salt
        end in
        let module S = DisjointDomain.PatriciaProjectiveSet (Printable.Strings) (B) (R) in
        let s = build S.add (S.empty ()) ["a"; "aa"; "ab"] in
        salt := 42;
        let s = S.relift s in
        List.iter (fun x -> assert_bool "find after relift" (S.mem x s)) ["a"; "aa"; "ab"];
        assert_equal 3 (S.cardinal s));
  ]
