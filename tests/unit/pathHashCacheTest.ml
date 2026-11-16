open Goblint_lib
open OUnit2

let hash_calls = ref 0
module Representative = struct
  include Printable.Strings
  type elt = t
  let of_elt x = x
  let hash x = incr hash_calls; String.length x mod 2
end

module Paths = MCPRegistry.DomListRepresentative (struct
    let assoc_dom _ = (module Representative : DisjointDomain.Representative)
    let domain_list () = [0, assoc_dom 0]
  end)

let path s = Paths.of_elt [0, Obj.repr s]

let tests = "path_hash_cache" >::: [
    "structural_order" >:: (fun _ ->
        List.iter (fun a -> List.iter (fun b ->
            assert_equal (Int.compare (String.compare a b) 0)
              (Int.compare (Paths.compare (path a) (path b)) 0)
          ) [""; "a"; "aa"; "ab"; "b"]) [""; "a"; "aa"; "ab"; "b"]);
    "cache_reuse" >:: (fun _ ->
        let a = path "aa" and b = path "ab" in
        hash_calls := 0;
        ignore (Paths.compare a b);
        assert_equal 0 !hash_calls;
        assert_equal (Paths.hash a) (Paths.hash b);
        assert_equal 2 !hash_calls;
        assert_bool "collisions remain distinct" (Paths.compare a b <> 0);
        ignore (Paths.hash a);
        assert_equal 2 !hash_calls;
        ignore (Paths.hash (Paths.relift a));
        assert_equal 3 !hash_calls);
    "marshal" >:: (fun _ ->
        let a = path "aa" in
        let before = Marshal.from_string (Marshal.to_string a []) 0 in
        let h = Paths.hash a in
        let after = Marshal.from_string (Marshal.to_string a []) 0 in
        List.iter (fun b ->
            assert_bool "equal after round trip" (Paths.equal a b);
            assert_equal h (Paths.hash b)
          ) [before; after]);
  ]
