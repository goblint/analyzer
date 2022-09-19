(** Call to [merge x y] returns json where [x] is updated with values in [y] *)
let rec merge x y =
  let module Object = BatMap.Make (String) in
  match x, y with
  | `Assoc m1, `Assoc m2 ->
    let merger k v1 v2 =
      match v1, v2 with
      | Some v1, Some v2 -> Some (merge v1 v2)
      | None   , Some v
      | Some v , None    -> Some v
      | None   , None    -> None
    in
    let nm = Object.bindings @@ Object.merge merger (m1 |> BatList.enum |> Object.of_enum) (m2 |> BatList.enum |> Object.of_enum) in
    `Assoc nm
  | `List l1, `List l2 ->
    let rec zipWith' x y =
      match x, y with
      | x::xs, y::ys    -> merge x y :: zipWith' xs ys
      | [], xs | xs, [] -> y
    in
    `List (zipWith' l1 l2)
  | _ -> y

let print (ch: 'a BatIO.output) json =
  let oo = object
    method output = BatIO.output_substring ch
  end
  in
  Yojson.Safe.to_output oo json

let pretty () json =
  GoblintCil.Pretty.text (Yojson.Safe.to_string json)
