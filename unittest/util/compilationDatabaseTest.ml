open OUnit2

let command_object_from_string s =
  s
  |> Yojson.Safe.from_string
  |> CompilationDatabase.command_object_of_yojson
  |> Result.get_ok

let test_split_arguments _ =
  let obj = command_object_from_string {json|
    {
      "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
      "arguments": [
        "gcc",
        "-DANSWER=42",
        "-Ilib/",
        "main.c",
        "lib/lib.c"
      ],
      "file": "lib/lib.c"
    }
    |json}
  in
  let actual_split = CompilationDatabase.split obj in
  let expected_split = List.map command_object_from_string [
      {json|
      {
        "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
        "arguments": [
          "gcc",
          "-DANSWER=42",
          "-Ilib/",
          "main.c"
        ],
        "file": "main.c"
      }
      |json};
      {json|
      {
        "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
        "arguments": [
          "gcc",
          "-DANSWER=42",
          "-Ilib/",
          "lib/lib.c"
        ],
        "file": "lib/lib.c"
      }
      |json};
    ]
  in
  assert_equal ~printer:CompilationDatabase.show expected_split actual_split

let test_split_command _ =
  let obj = command_object_from_string {json|
    {
      "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
      "command": "gcc -DANSWER=42 -Ilib/ main.c lib/lib.c",
      "file": "lib/lib.c"
    }
    |json}
  in
  let actual_split = CompilationDatabase.split obj in
  let expected_split = List.map command_object_from_string [
      {json|
      {
        "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
        "command": "gcc -DANSWER=42 -Ilib/ main.c",
        "file": "main.c"
      }
      |json};
      {json|
      {
        "directory": "/home/simmo/Desktop/goblint-action-test/makefile-project",
        "command": "gcc -DANSWER=42 -Ilib/ lib/lib.c",
        "file": "lib/lib.c"
      }
      |json};
    ]
  in
  assert_equal ~printer:CompilationDatabase.show expected_split actual_split

let tests =
  "compilationDatabaseTest" >::: [
    "split" >::: [
      "arguments" >:: test_split_arguments;
      "command" >:: test_split_command;
    ];
  ]
