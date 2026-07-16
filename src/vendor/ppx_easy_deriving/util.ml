let reduce ~unit ~both = function
  | [] -> unit
  | [x] -> x
  | xs ->
    let xs = List.rev xs in
    match xs with
    | x :: xs ->
      List.fold_right both (List.rev xs) x (* omits hash_empty *)
    | [] -> assert false

let map3 f l1 l2 l3 =
  List.map2 (fun x1 (x2, x3) -> f x1 x2 x3) l1 (List.combine l2 l3) (* TODO: optimize *)
