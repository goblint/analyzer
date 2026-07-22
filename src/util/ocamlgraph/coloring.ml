module Make (G : Graph.Coloring.G) = struct
  module C = Graph.Coloring.Make (G)
  module IntSet = Set.Make (Int)

  type coloring = int C.H.t

  module type ALGORITHM = sig
    val color : G.t -> coloring
  end

  let k_color g k = C.coloring g k
  let color_with (module A : ALGORITHM) g = A.color g
  let color_of coloring v = C.H.find_opt coloring v
  let colors_used coloring = C.H.fold (fun _ c acc -> max acc c) coloring 0

  module Greedy : ALGORITHM = struct
    let color g =
      let n = G.nb_vertex g in
      let coloring = C.H.create n in
      let vertices =
        G.fold_vertex (fun v acc -> (G.out_degree g v, v) :: acc) g []
        |> List.sort (fun (d1, _) (d2, _) -> compare d2 d1)
        |> List.map snd
      in
      let next_color v =
        let used = ref IntSet.empty in
        G.iter_succ (fun u ->
            match C.H.find_opt coloring u with
            | None -> ()
            | Some c -> used := IntSet.add c !used
          ) g v;
        let rec pick c =
          if IntSet.mem c !used then
            pick (c + 1)
          else
            c
        in
        pick 1
      in
      List.iter (fun v -> C.H.add coloring v (next_color v)) vertices;
      coloring
  end

  module Optimal : ALGORITHM = struct
    let color g =
      let max_colors = max 1 (G.nb_vertex g) in
      let rec loop k =
        if k > max_colors then
          raise Graph.Coloring.NoColoring
        else
          try C.coloring g k with
          | Graph.Coloring.NoColoring -> loop (k + 1)
      in
      loop 1
  end

  module Dsatur : ALGORITHM = struct
    let color g =
      let n = G.nb_vertex g in
      let coloring = C.H.create n in
      let saturation = C.H.create n in
      let degree = C.H.create n in
      G.iter_vertex (fun v ->
          C.H.replace saturation v IntSet.empty;
          C.H.replace degree v (G.out_degree g v)
        ) g;
      let is_colored v = C.H.mem coloring v in
      let sat_count v =
        match C.H.find_opt saturation v with
        | None -> 0
        | Some s -> IntSet.cardinal s
      in
      let choose_vertex () =
        let pick v best_opt =
          if is_colored v then
            best_opt
          else
            match best_opt with
            | None -> Some v
            | Some best ->
              let sv = sat_count v in
              let sb = sat_count best in
              if sv > sb then
                Some v
              else if sv < sb then
                Some best
              else (
                let dv = C.H.find degree v in
                let db = C.H.find degree best in
                if dv > db then Some v else Some best
              )
        in
        G.fold_vertex pick g None
      in
      let pick_color v =
        let used =
          match C.H.find_opt saturation v with
          | None -> IntSet.empty
          | Some s -> s
        in
        let rec pick c =
          if IntSet.mem c used then
            pick (c + 1)
          else
            c
        in
        pick 1
      in
      let rec loop () =
        match choose_vertex () with
        | None -> ()
        | Some v ->
          let c = pick_color v in
          C.H.add coloring v c;
          G.iter_succ (fun u ->
              if not (is_colored u) then
                let s =
                  match C.H.find_opt saturation u with
                  | None -> IntSet.empty
                  | Some s -> s
                in
                C.H.replace saturation u (IntSet.add c s)
            ) g v;
          loop ()
      in
      loop ();
      coloring
  end

  module Rlf : ALGORITHM = struct
    module VSet = struct
      let create n = C.H.create n
      let mem s v = C.H.mem s v
      let add s v = C.H.replace s v ()
      let remove s v = C.H.remove s v
    end

    let color g =
      let n = G.nb_vertex g in
      let coloring = C.H.create n in
      let uncolored = VSet.create n in
      G.iter_vertex (fun v -> VSet.add uncolored v) g;
      let degree v = G.out_degree g v in
      let pick_start () =
        G.fold_vertex (fun v best ->
            if not (VSet.mem uncolored v) then
              best
            else
              match best with
              | None -> Some v
              | Some b ->
                if degree v > degree b then Some v else Some b
          ) g None
      in
      let add_forbidden forbidden v =
        G.iter_succ (fun u ->
            if VSet.mem uncolored u then
              VSet.add forbidden u
          ) g v
      in
      let candidate_score forbidden v =
        let count = ref 0 in
        G.iter_succ (fun u ->
            if VSet.mem forbidden u then
              incr count
          ) g v;
        !count
      in
      let pick_candidate forbidden =
        G.fold_vertex (fun v best ->
            if not (VSet.mem uncolored v) || VSet.mem forbidden v then
              best
            else
              match best with
              | None -> Some v
              | Some b ->
                let sv = candidate_score forbidden v in
                let sb = candidate_score forbidden b in
                if sv > sb then
                  Some v
                else if sv < sb then
                  Some b
                else if degree v > degree b then
                  Some v
                else
                  Some b
          ) g None
      in
      let rec color_class color =
        match pick_start () with
        | None -> ()
        | Some v0 ->
          let forbidden = VSet.create n in
          let add_vertex v =
            C.H.add coloring v color;
            VSet.remove uncolored v;
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
          color_class (color + 1)
      in
      color_class 1;
      coloring
  end
end
