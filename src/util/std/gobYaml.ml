let to_string' ?(len=65535 * 4) ?encoding ?scalar_style ?layout_style v =
  assert (len >= 1);
  let rec aux len =
    match Yaml.to_string ~len ?encoding ?scalar_style ?layout_style v with
    | Ok _ as o -> o
    | Error (`Msg ("scalar failed" | "doc_end failed" | "seq_end failed" | "mapping_start failed")) when len < Sys.max_string_length / 2 ->
      aux (len * 2)
    | Error (`Msg _) as e -> e
  in
  aux len

include Yaml.Util

include GobResult.Syntax

let option_map (f: 'a -> ('b, 'e) result) (o: 'a option): ('b option, 'e) result =
  match o with
  | Some x -> Result.map Option.some (f x)
  | None -> Ok None

let rec list_map (f: 'a -> ('b, 'e) result) (l: 'a list): ('b list, 'e) result =
  match l with
  | [] -> Ok []
  | x :: xs ->
    let+ y = f x
    and+ ys = list_map f xs in
    y :: ys

let find s y =
  match Yaml.Util.find s y with
  | Ok (Some y'') -> Ok y''
  | Ok None -> Error (`Msg ("find " ^ s))
  | Error `Msg e ->
    Error (`Msg ("find " ^ s ^ ": " ^ e))

let to_int y =
  let+ f = to_float y in
  int_of_float f

let list = function
  | `A l -> Ok l
  | _ -> Error (`Msg "Failed to get elements from non-array value")

let entries = function
  | `O assoc -> Ok assoc
  | _ -> Error (`Msg "Failed to get entries from non-object value")

let int i = float (float_of_int i)
