open GobConfig
open Pretty
open PrecisionUtil

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating an flaot value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)

  (** {b Comparison operators} *)
  val lt : t -> t -> IntDomain.IntDomTuple.t
  (** Less than: [x < y] *)
  val gt : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than: [x > y] *)
  val le : t -> t -> IntDomain.IntDomTuple.t
  (** Less than or equal: [x <= y] *)
  val ge : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than or equal: [x >= y] *)
  val eq : t -> t -> IntDomain.IntDomTuple.t
  (** Equal to: [x == y] *)
  val ne : t -> t -> IntDomain.IntDomTuple.t
  (** Not equal to: [x != y] *)
end

module type FloatDomainBase = sig
  include Lattice.S
  include FloatArith with type t := t

  val of_const : float -> t
end

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

module FloatInterval = struct
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

module FloatDomImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)
  module F1 = FloatInterval
  open Batteries

  type t = F1.t option [@@deriving to_yojson, eq, ord]

  let name () = "FloatInterval"

  type 'a m = (module FloatDomainBase with type t = 'a)
  (* only first-order polymorphism on functions 
     -> use records to get around monomorphism restriction on arguments (Same trick as used in intDomain) *)
  type 'b poly_in = { fi : 'a. 'a m -> 'b -> 'a }
  type 'b poly_pr = { fp : 'a. 'a m -> 'a -> 'b }
  type 'b poly2_pr = { f2p : 'a. 'a m -> 'a -> 'a -> 'b }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a }
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }

  let create r x (f1 : float_precision) =
    let f b g = if b then Some (g x) else None in
    f f1 @@ r.fi (module F1)

  let create r x =
    (* use where values are introduced *)
    create r x (float_precision_from_config ())

  let opt_map2 f =
    curry @@ function Some x, Some y -> Some (f x y) | _ -> None

  let exists = function Some true -> true | _ -> false
  let for_all = function Some false -> false | _ -> true

  let mapp r = BatOption.map (r.fp (module F1))

  let map r a = BatOption.(map (r.f1 (module F1)) a)
  let map2 r xa ya = opt_map2 (r.f2 (module F1)) xa ya
  let map2p r xa ya = opt_map2 (r.f2p (module F1)) xa ya

  let map2int r xa ya =
    Option.map_default identity
      (IntDomain.IntDomTuple.top_of IBool) (opt_map2 (r.f2p (module F1)) xa ya)

  let ( %% ) f g x = f % g x

  let show x =
    Option.map_default identity ""
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) x -> F.name () ^ ":" ^ F.show x); } x)

  let to_yojson =
    [%to_yojson: Yojson.Safe.t option]
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.to_yojson); }
  let hash x =
    Option.map_default identity 0
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.hash); } x)

  let of_const =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.of_const); }

  let top =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.top); }
  let bot =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.bot); }
  let is_bot =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.is_bot); }
  let is_top =
    for_all
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.is_top); }


  let leq =
    for_all
    %% map2p { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.leq); }

  let pretty () x =
    Option.map_default identity nil
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.pretty ()); } x)

  (* f1: one and only unary op *)
  let neg =
    map { f1= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.neg); }

  (* f2: binary ops *)
  let join =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.join); }
  let meet =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.meet); }
  let widen =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.widen); }
  let narrow =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.narrow); }
  let add =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.add); }
  let sub =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.sub); }
  let mul =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.mul); }
  let div =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.div); }

  (* f2: binary ops which return an integer *)
  let lt =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.lt); }
  let gt =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.gt); }
  let le =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.le); }
  let ge =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.ge); }
  let eq =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.eq); }
  let ne =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.ne); }

  let pretty_diff () (x, y) = dprintf "%a instead of %a" pretty x pretty y

  let printXml f x =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)
end
