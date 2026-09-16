open OUnit2

module Int =
struct
  include Int
  let hash (x: int) = Hashtbl.hash x
end

module G =
struct
  module G0 =
  struct
    include Graph.Imperative.Graph.Concrete (Int) (* Imperative seems a bit faster *)

    (* For show: *)
    let vertex_name = Int.to_string
    let graph_attributes _ = []
    let vertex_attributes v = []
    let edge_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
  end

  include G0

  module Dot = Graph.Graphviz.Neato (G0)
  let show = Format.asprintf "%a" Dot.fprint_graph

  let gen =
    let module R = Graph.Rand.I (G0) in
    let open QCheck2.Gen in
    sized_size nat_small (fun v ->
        let v = v + 1 in (* nat_small includes 0, but R.graph doesn't like it *)
        let* e = int_bound (v * (v - 1) / 2) in
        pure (R.graph ~loops:false ~v ~e ())
      )
end

module C = Goblint_ocamlgraph.Coloring.Make (G)

module Make (Algorithm: C.Algorithm) =
struct
  let test_valid_coloring =
    QCheck2.Test.make ~name:"valid coloring" G.gen ~print:G.show (fun g ->
        let c = C.Greedy.color g in
        C.valid_coloring g c
      ) |> QCheck_ounit.to_ounit2_test

  let tests = [
    test_valid_coloring;
  ]
end

let algorithms = [
  ("optimal", (module C.Optimal: C.Algorithm));
  ("greedy", (module C.Greedy));
  ("dsatur", (module C.Dsatur));
  ("rlf", (module C.Rlf));
]

let tests =
  "coloringTest" >:::
    List.map (fun (name, (module Algorithm: C.Algorithm)) ->
        let module AlgorithmTest = Make (Algorithm) in
        name >::: AlgorithmTest.tests
      ) algorithms
