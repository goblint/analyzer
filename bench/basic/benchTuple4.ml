(* dune exec bench/basic/benchTuple4.exe -- -a *)

open Benchmark
open Benchmark.Tree


let () =
  let exists0 =
    let open Batteries in
    let to_list x = Tuple4.enum x |> List.of_enum |> List.filter_map identity in
    let f g = g identity % to_list in
    List.(f exists)
  in

  let exists1 = function
    | (Some true, _, _, _)
    | (_, Some true, _, _)
    | (_, _, Some true, _)
    | (_, _, _, Some true) ->
      true
    | _ ->
      false
  in

  let exists2 (a, b, c, d) = a = Some true || b = Some true || c = Some true || d = Some true in


  register (
    "exists" @>>> [
        "all None" @> lazy (
            let args = (None, None, None, None) in
            throughputN 1 [
              ("0", exists0, args);
              ("1", exists1, args);
              ("2", exists2, args);
            ]
          );
        "all Some true" @> lazy (
            let args = (Some true, Some true, Some true, Some true) in
            throughputN 1 [
              ("0", exists0, args);
              ("1", exists1, args);
              ("2", exists2, args);
            ]
          );
        "all Some false" @> lazy (
            let args = (Some false, Some false, Some false, Some false) in
            throughputN 1 [
              ("0", exists0, args);
              ("1", exists1, args);
              ("2", exists2, args);
            ]
          );
        "all None except last Some true" @> lazy (
            let args = (None, None, None, Some true) in
            throughputN 1 [
              ("0", exists0, args);
              ("1", exists1, args);
              ("2", exists2, args);
            ]
          );
        "all Some false except last Some true" @> lazy (
            let args = (Some false, Some false, Some false, Some true) in
            throughputN 1 [
              ("0", exists0, args);
              ("1", exists1, args);
              ("2", exists2, args);
            ]
          );
      ]
  )

let () =
  run_global ()
