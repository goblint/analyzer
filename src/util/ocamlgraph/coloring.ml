module Color = Int

module ColorSet =
struct
  include Set.Make (Color)

  let find_unused used =
    (* TODO: could be imperatively optimized, or with DIET data structure *)
    let rec loop c =
      if mem c used then
        loop (c + 1)
      else
        c
    in
    loop 1
end

module Make (G: Graph.Coloring.G) =
struct
  module H = Hashtbl.Make (G.V)
  type coloring = Color.t H.t

  let valid_coloring (g: G.t) (c: coloring) =
    try
      G.iter_vertex (fun u ->
          let cu = H.find c u in
          G.iter_succ (fun v ->
              let cv = H.find c v in
              if Color.equal cu cv then
                raise_notrace Stdlib.Exit
            ) g u
        ) g;
      true
    with Stdlib.Exit ->
      false

  module type Algorithm =
  sig
    val color: G.t -> coloring
  end

  module Greedy: Algorithm =
  struct
    let color g =
      let n = G.nb_vertex g in
      let coloring = H.create n in
      let vertex_order =
        G.fold_vertex (fun v acc -> (v, G.out_degree g v) :: acc) g []
        |> List.sort (fun (_, d1) (_, d2) -> compare d2 d1) (* compare argument swapped to sort highest degree first! *)
        |> List.map fst
      in
      let pick_color v =
        (* TODO: use saturation hashtbl like in Dsatur? *)
        let used = G.fold_succ (fun u used ->
            if G.V.equal v u then (* loop *)
              raise Graph.Coloring.NoColoring;
            match H.find_opt coloring u with
            | None -> used
            | Some c -> ColorSet.add c used
          ) g v ColorSet.empty
        in
        ColorSet.find_unused used
      in
      List.iter (fun v -> H.replace coloring v (pick_color v)) vertex_order;
      coloring
  end

  module Optimal: Algorithm =
  struct
    module C = Graph.Coloring.Make (G)

    let color g =
      let n = G.nb_vertex g in
      let rec loop k =
        if k > n then (* loop *)
          raise Graph.Coloring.NoColoring;
        try C.coloring g k
        with Graph.Coloring.NoColoring -> loop (k + 1)
      in
      loop 1
  end

  module Dsatur: Algorithm =
  struct
    let color g =
      let n = G.nb_vertex g in
      let coloring = H.create n in
      let succ_used = H.create n in
      let degree = H.create n in
      let uncolored = H.create n in (* TODO: use actual priority queue? *)
      G.iter_vertex (fun v ->
          H.replace succ_used v ColorSet.empty;
          H.replace degree v (G.out_degree g v);
          H.replace uncolored v ();
        ) g;
      let saturation v =
        ColorSet.cardinal (H.find succ_used v)
      in
      let pick_vertex () =
        let pick v () best_opt =
          match best_opt with
          | None -> Some v
          | Some best ->
            let sv = saturation v in
            let sb = saturation best in
            if sv > sb then
              Some v
            else if sv < sb then
              Some best
            else (
              let dv = H.find degree v in
              let db = H.find degree best in
              if dv > db then Some v else Some best
            )
        in
        H.fold pick uncolored None
      in
      let pick_color v =
        let used = H.find succ_used v in
        ColorSet.find_unused used
      in
      let rec loop () =
        match pick_vertex () with
        | None -> ()
        | Some v ->
          let c = pick_color v in
          H.replace coloring v c;
          H.remove uncolored v;
          G.iter_succ (fun u ->
              if G.V.equal v u then (* loop *)
                raise Graph.Coloring.NoColoring;
              if H.mem uncolored u then (
                let used = H.find succ_used u in
                H.replace succ_used u (ColorSet.add c used);
                let d = H.find degree u in
                H.replace degree u (d - 1)
              )
            ) g v;
          loop ()
      in
      loop ();
      coloring
  end

  module Rlf: Algorithm =
  struct
    let color g =
      let n = G.nb_vertex g in
      let coloring = H.create n in
      let uncolored = H.create n in
      G.iter_vertex (fun v -> H.replace uncolored v ()) g;
      let degree v = G.out_degree g v in (* TODO: degrees should reduce like in DSatur? *)
      let pick_start () =
        H.fold (fun v () best_opt ->
            match best_opt with
            | None -> Some v
            | Some best ->
              if degree v > degree best then Some v else Some best (* best is highest *)
          ) uncolored None
      in
      let add_forbidden forbidden v =
        G.iter_succ (fun u ->
            if G.V.equal v u then (* loop *)
              raise Graph.Coloring.NoColoring;
            if H.mem uncolored u then
              H.replace forbidden u ()
          ) g v
      in
      let forbidden_succs forbidden v =
        G.fold_succ (fun u acc ->
            if H.mem forbidden u then
              acc + 1
            else
              acc
          ) g v 0
      in
      let pick_candidate forbidden =
        H.fold (fun v () best_opt ->
            if H.mem forbidden v then
              best_opt
            else
              match best_opt with
              | None -> Some v
              | Some best ->
                let sv = forbidden_succs forbidden v in
                let sb = forbidden_succs forbidden best in
                if sv > sb then
                  Some v
                else if sv < sb then
                  Some best
                else if degree v > degree best then (* TODO: best should be lowest here *)
                  Some v
                else
                  Some best
          ) uncolored None
      in
      let rec color_class color =
        match pick_start () with
        | None -> ()
        | Some v0 ->
          let forbidden = H.create n in
          let add_vertex v =
            H.replace coloring v c;
            H.remove uncolored v;
            add_forbidden forbidden v
          in
          add_vertex v0;
          let rec fill () =
            match pick_candidate forbidden with
            | None -> ()
            | Some v ->
              add_vertex v;
              fill ()
          in
          fill ();
          color_class (c + 1)
      in
      color_class 1;
      coloring
  end
end
