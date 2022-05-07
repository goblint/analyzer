open Pretty

let eval_binop operation op1 op2 =
  match (op1, op2) with Some v1, Some v2 -> operation v1 v2 | _ -> None

let eval_int_binop operation op1 op2 =
  let a, b =
    match (op1, op2) with Some v1, Some v2 -> operation v1 v2 | _ -> (0, 1)
  in
  IntDomain.IntDomTuple.of_interval IBool
    (Big_int_Z.big_int_of_int a, Big_int_Z.big_int_of_int b)

let add (low1, high1) (low2, high2) = Some (low1 +. low2, high1 +. high2)

let sub (low1, high1) (low2, high2) = Some (low1 -. low2, high1 -. high2)

let mul (low1, high1) (low2, high2) =
  let mul1 = low1 *. low2 in
  let mul2 = low1 *. high2 in
  let mul3 = high1 *. low2 in
  let mul4 = high1 *. high2 in
  let low = min (min (min mul1 mul2) mul3) mul4 in
  let high = max (max (max mul1 mul2) mul3) mul4 in
  Some (low, high)

let div (low1, high1) (low2, high2) =
  if low2 < 0. && high2 > 0. then None
  else
    let mul1 = low1 /. low2 in
    let mul2 = low1 /. high2 in
    let mul3 = high1 /. low2 in
    let mul4 = high1 /. high2 in
    let low = min (min (min mul1 mul2) mul3) mul4 in
    let high = max (max (max mul1 mul2) mul3) mul4 in
    Some (low, high)

let eq (low1, high1) (low2, high2) =
  if high1 < low2 || high2 < low1 then (0, 0)
  else if high1 = low1 && high2 = low2 && low1 = low2 then (1, 1)
  else (0, 1)

let ne (low1, high1) (low2, high2) =
  if high1 < low2 || high2 < low1 then (1, 1)
  else if high1 == low1 && high2 == low2 && low1 == low2 then (0, 0)
  else (0, 1)

let lt (low1, high1) (low2, high2) =
  if high1 < low2 then (1, 1) else if low1 > high2 then (0, 0) else (0, 1)

let gt (low1, high1) (low2, high2) =
  if low1 > high2 then (1, 1) else if high1 < low2 then (0, 0) else (0, 1)

module FloatDomTuple = struct
  type t = (float * float) option [@@deriving eq, to_yojson]

  let of_const f = Some (f, f)

  let hash = Hashtbl.hash

  let compare _ _ = failwith "todo"

  let show = function
    | None ->
        "Float [arbitrary]"
    | Some (low, high) ->
        "Float [" ^ string_of_float low ^ "," ^ string_of_float high ^ "]"

  let pretty () x = text (show x)

  let printXml f (x : t) =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let name () = "float interval domain"

  let invariant _ (x : t) = failwith "todo"

  let tag (x : t) = failwith "todo"

  let arbitrary () = failwith "todo"

  let relift (x : t) = failwith "todo"

  let leq _ _ = failwith "todo"

  let join _ _ = failwith "todo"

  let meet _ _ = failwith "todo"

  (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)
  let widen _ _ = failwith "todo"

  let narrow _ _ = failwith "todo"

  (** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
  let pretty_diff () (x, y) =
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let bot () = failwith "no bot exists"

  let is_bot _ = failwith "no bot exists"

  let top () = None

  let is_top = Option.is_none

  let neg = Option.map (fun (low, high) -> (-.high, -.low))

  let add = eval_binop add

  let sub = eval_binop sub

  let mul = eval_binop mul

  let div = eval_binop div

  let lt = eval_int_binop lt

  let gt = eval_int_binop gt

  let le _ _ = failwith "todo - le"

  let ge _ _ = failwith "todo - ge"

  let eq = eval_int_binop eq

  let ne = eval_int_binop ne
end
