module Utilities =
  struct
    (* Cartesian Product (http://stackoverflow.com/q/1507496) *)
    let cartesian_product xs ys =
      List.fold_left 
        (fun acc x -> 
          List.fold_left
            (fun acc y -> (x,y) :: acc)
            acc ys)
        [] xs;;

    (* Map on two Lists *)
    let cartesian_map f xs ys =
      List.fold_left 
        (fun acc x -> 
          List.fold_left
            (fun acc y -> (f x y) :: acc)
            acc ys)
        [] xs;;

    (* Create Range from zero to k-1 *)
    let range k =
      let rec f n l =
        if n <= 0 then (0::l) else f (n-1) (n::l)
      in
      f (k-1) [];;
  end
