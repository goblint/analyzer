open OUnit2

let test_merge_ignores_schema _ =
  let original = GobConfig.get_conf () in
  Fun.protect
    ~finally:(fun () -> GobConfig.set_conf original)
    (fun () ->
       GobConfig.merge (`Assoc [
           ("$schema", `String "https://example.com/goblint.schema.json");
           ("jobs", `Int 2);
         ]);
       assert_equal 2 (GobConfig.get_int "jobs");
       match GobConfig.get_conf () with
       | `Assoc fields -> assert_bool "$schema should not be stored" (not (List.mem_assoc "$schema" fields))
       | _ -> assert_failure "configuration should be an object"
    )

let tests =
  "gobConfig" >::: [
    "merge ignores $schema" >:: test_merge_ignores_schema;
  ]
