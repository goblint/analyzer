open Goblint_lib
open OUnit2

let tests = "dispatch_cache" >::: [
    "source_changes" >:: (fun _ ->
        let source = ref [] in
        let calls = ref 0 in
        let cached = MCPRegistry.cached_dispatch_map (fun x -> incr calls; x + 1) (fun () -> !source) in
        assert_equal [] (cached ());
        source := [1; 2];
        let first = cached () in
        assert_equal [2; 3] first;
        assert_bool "reuse unchanged source" (first == cached ());
        assert_equal 2 !calls;
        source := [3];
        assert_equal [4] (cached ());
        assert_equal 3 !calls;
        source := [];
        assert_equal [] (cached ()));
  ]
