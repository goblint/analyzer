type t = string option [@@deriving eq, ord, hash]

let hash x =
  if GobConfig.get_bool "ana.base.limit-string-addresses" then
    13859
  else
    hash x

let show = function
  | Some x -> "\"" ^ x ^ "\""
  | None -> "(unknown string)"

include Printable.SimpleShow (
  struct
    type nonrec t = t
    let show = show
  end
  )

let of_string x = Some x
let to_string x = x

(* only keep part before first null byte *)
let to_c_string = function
  | Some x ->
    begin match String.split_on_char '\x00' x with
      | s::_ -> Some s
      | [] -> None
    end
  | None -> None

let to_n_c_string n x =
  match to_c_string x with
  | Some x ->
    if n > String.length x then
      Some x
    else if n < 0 then
      None
    else
      Some (String.sub x 0 n)
  | None -> None

let to_string_length x =
  match to_c_string x with
  | Some x -> Some (String.length x)
  | None -> None

let to_exp = function
  | Some x -> GoblintCil.mkString x
  | None -> raise (Lattice.Unsupported "Cannot express unknown string pointer as expression.")

let semantic_equal x y =
  match x, y with
  | None, _
  | _, None -> Some true
  | Some a, Some b -> if a = b then None else Some false

let leq x y =
  match x, y with
  | _, None -> true
  | a, b   -> a = b

let join x y =
  match x, y with
  | None, _
  | _, None -> None
  | Some a, Some b when a = b -> Some a
  | Some a, Some b (* when a <> b *) ->
    if GobConfig.get_bool "ana.base.limit-string-addresses" then
      None
    else
      raise Lattice.Uncomparable

let meet x y =
  match x, y with
  | None, a
  | a, None -> a
  | Some a, Some b when a = b -> Some a
  | Some a, Some b (* when a <> b *) ->
    if GobConfig.get_bool "ana.base.limit-string-addresses" then
      raise Lattice.BotValue
    else
      raise Lattice.Uncomparable

let repr x =
  if GobConfig.get_bool "ana.base.limit-string-addresses" then
    None (* all strings together if limited *)
  else
    x (* everything else is kept separate, including strings if not limited *)
