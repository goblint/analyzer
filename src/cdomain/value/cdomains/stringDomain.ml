include Printable.StdLeaf

let name () = "string"

type string_domain = Unit | Disjoint | Flat

let string_domain: string_domain ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      match GobConfig.get_string "ana.base.strings.domain" with
      | "unit" -> Unit
      | "disjoint" -> Disjoint
      | "flat" -> Flat
      | _ -> failwith "ana.base.strings.domain: illegal value"
    )

let get_string_domain () = ResettableLazy.force string_domain

let reset_lazy () =
  ResettableLazy.reset string_domain


type t = string option [@@deriving eq, ord, hash]
(** [None] means top. *)

let hash x =
  match get_string_domain () with
  | Disjoint | Flat -> hash x
  | Unit -> 13859

let show = function
  | Some x -> "\"" ^ x ^ "\""
  | None -> "(unknown string)"

include Printable.SimpleShow (
  struct
    type nonrec t = t
    let show = show
  end
  )

let of_string x =
  match get_string_domain () with
  | Unit -> None
  | Disjoint | Flat -> Some x
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
      Some (Str.first_chars x n)
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
    match get_string_domain () with
    | Disjoint -> raise Lattice.Uncomparable
    | Flat -> None
    | Unit -> assert false

let meet x y =
  match x, y with
  | None, a
  | a, None -> a
  | Some a, Some b when a = b -> Some a
  | Some a, Some b (* when a <> b *) ->
    match get_string_domain () with
    | Disjoint -> raise Lattice.Uncomparable
    | Flat -> raise Lattice.BotValue
    | Unit -> assert false

let repr x =
  match get_string_domain () with
  | Disjoint ->
    x (* everything else is kept separate, including strings if not limited *)
  | Flat | Unit ->
    None (* all strings together if limited *)
