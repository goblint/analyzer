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
  Pretty.text (Yojson.Safe.to_string json)

(* Copied from https://github.com/ocaml-community/yojson/blob/7fbec1d96a77b63a78b9e5f92d78648cdc60f220/lib/read.mll#L1146-L1158. *)
(* TODO: remove when Yojson 2.0.0 is released *)
let seq_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
  let stream = Some true in
  let rec f () =
    try Seq.Cons (Yojson.Safe.from_lexbuf v ?stream lexbuf, f)
    with
        Yojson.End_of_input ->
          fin ();
          Seq.Nil
      | e ->
          (try fin () with fin_e -> raise (Yojson.Safe.Finally (e, fin_e)));
          raise e
  in
  f
