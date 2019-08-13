open OUnit

let was_test_successful x = 
  match x with
      RSuccess y -> true
    | _ -> false

let was_successful x = 
  List.fold_left (&&) true (List.map was_test_successful x)
