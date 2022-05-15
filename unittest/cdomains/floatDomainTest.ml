open OUnit2

let exampleTest1 _ =
  assert_equal 0 0;
  assert_equal 1 1

module ExampleTestModule2 =
struct

  let exampleTest2 _ =
  assert_equal 0 0;
  assert_equal 1 1

  let test () = [
    "exampleTestModule2" >:: exampleTest2;
  ]
end

let test () = "floatDomainTest" >:::
              [ "Example Test 1"  >:: exampleTest1;
                "Example Test 2"  >::: ExampleTestModule2.test()
              ]