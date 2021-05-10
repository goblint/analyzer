module BI = IntOps.BigIntOps

module IKind = struct let ikind () = Cil.IInt end
module INV : IntDomain.Y with type int_t = BI.t = IntDomain.IntDomWithDefaultIkind (IntDomain.IntDomLifter (IntDomain.Interval)) (IKind)

  type elt = | Val of float | Infinity
  [@@deriving yojson]

  let elt_to_string elt =
    match elt with
    | Infinity -> "inf"
    | Val f -> string_of_float f

let oct_ik = IKind.ikind ()

module type S = sig
  include Lattice.S
  val of_array : float array array -> t
  val set_constraint : t -> (bool * int) option * bool * int * bool * elt -> t
  val set_var_bounds : t -> int -> elt * elt -> t
  val adjust_variable : t -> int -> float -> t
  val adjust_variables : t -> int -> int -> float -> t
  val projection : t -> int -> elt * elt
  val constraints : t -> string
  val to_string_matrix : t -> string
  val strong_closure : t -> t
  val top_of_size : int -> t
  val copy_oct : t -> t
end

module ArrayOctagon : (S with type t = elt array array) =
struct
  let copy_oct oct =
    Array.map Array.copy oct
  (* TODO: implement better narrow function *)
  include Printable.Blank

  type t = elt array array
  [@@deriving yojson]

  let of_array matrix =
    Array.map (fun arr -> Array.map (fun elt -> Val elt) arr) matrix

  let size oct = (Array.length oct) / 2

  let get_elt oct i j = Array.get (Array.get oct i) j
  let set_elt oct i j c =
    Array.set (Array.get oct i) j c

  let add a b =
    match a, b with
    | Infinity , _ | _ , Infinity -> Infinity
    | Val a, Val b -> Val (a +. b)

  let smaller a b =
    match a, b with
    | Infinity , _ -> false
    | _ , Infinity -> true
    | Val a, Val b -> a < b

  let elt_leq a b =
    match a, b with
    | Infinity , _ -> false
    | _ , Infinity -> true
    | Val a, Val b -> a <= b


  let oct_find2 filter oct1 oct2 =
    if size oct1 <> size oct2
    then raise Lattice.Uncomparable
    else
      let oct_size = Array.length oct1 in
      let rec find i j =
        if i >= oct_size then None else
          let first = get_elt oct1 i j in
          let second = get_elt oct2 i j in
          if filter first second
          then
            Some (first, second)
          else
            let j = j + 1 in
            if j >= oct_size then
              find (i + 1) 0
            else
              find i j
      in
      find 0 0

  let equal oct1 oct2 =
    (match oct_find2 (fun a b -> a != b) oct1 oct2 with
     | Some _ -> false
     | None -> true)

  let hash oct = 2

  let compare oct1 oct2 =
    match oct_find2 (fun a b -> a < b) oct1 oct2,
          oct_find2 (fun a b -> a > b) oct1 oct2 with
    | None, None
    | Some _, Some _ -> 0
    | None, Some _ -> 1
    | Some _, None -> -1

  let name () = "Octagon Domain"

  let leq oct1 oct2 =
    (compare oct1 oct2) = -1

  let oct_map2 fn oct1 oct2 =
    Array.map2 (fun inner1 inner2 ->
        Array.map2 (fun elt1 elt2 -> fn elt1 elt2)
          inner1 inner2)
      oct1 oct2

  let oct_mapi fn oct =
    Array.mapi (fun i inner ->
        Array.mapi (fun j elt ->
            fn i j elt) inner) oct

  let inverse_index i = i lxor 1

  let min_elt a b =
    match a, b with
    | Infinity, x | x, Infinity -> x
    | Val a, Val b -> Val (min a b)

  let strong_closure oct =
    (* S+ matrix of the octagon paper *)

    let s oct =
      Array.mapi (fun i inner ->
          Array.mapi (fun j elt ->
              let first = get_elt oct i (inverse_index i) in
              let second = get_elt oct (inverse_index j) j in
              let secondval = match first, second with
                | Val f, Val s -> Val ((f +. s) /. 2.0)
                | _ -> Infinity in
              min_elt elt secondval
            ) inner) oct
    in

    let c oct k =
      (* C+ matrix of the octagon paper *)
      Array.mapi (fun i inner ->
          Array.mapi (fun j elt ->
              if i == j then Val 0.0 else
                let a = (add (get_elt oct i k) (get_elt oct k j)) in
                let b = (add (get_elt oct i (inverse_index k)) (get_elt oct (inverse_index k) j)) in
                let c = (add (add (get_elt oct i k) (get_elt oct k (inverse_index k))) (get_elt oct (inverse_index k) j)) in
                let d = (add (add (get_elt oct i (inverse_index k)) (get_elt oct (inverse_index k) k)) (get_elt oct k j)) in
                let new_val = List.fold_left min_elt Infinity [elt; a; b; c; d] in
                match new_val with
                | Val -0. -> Val 0.
                | _ -> new_val
            ) inner ) oct
    in

    let rec strong_closure' oct k =
      let oct_size = size oct in
      if k < oct_size then
        strong_closure' (s (c oct (2 * k ))) (k + 1)
      else
        oct
    in
    strong_closure' oct 0

  let min a b =
    match a, b with
    | Infinity , _ -> b
    | _ , Infinity -> a
    | Val a, Val b -> Val (min a b)

  let max a b =
    match a, b with
    | Infinity , _ -> a
    | _ , Infinity -> b
    | Val a, Val b -> Val (max a b)

  let join oct1 oct2 = strong_closure (oct_map2 max oct1 oct2) |> copy_oct

  let to_string_matrix oct =
    Array.fold_left
      (fun s inner -> s ^ "[" ^ (Array.fold_left (fun s elt -> s ^ (elt_to_string elt) ^ " ") "" inner) ^ "]\n") "" oct

  let meet oct1 oct2 =
    (oct_map2 min oct1 oct2) |> copy_oct

  let widen oct1 oct2 =
    let oct1 = copy_oct oct1 in
    let oct2 = copy_oct oct2 in
    let a =
      (oct_map2
         (fun a b ->
            if elt_leq b a
            then a
            else Infinity)
         oct1 oct2)
    in
    copy_oct a


  let narrow a b =
    copy_oct a

  let top_of_size size =
    let size = size * 2 in
    Array.make_matrix size size Infinity

  let bot_of_size size =
    let size = size * 2 in
    Array.make_matrix size size (Val 0.)

  let is_top oct = not (Array.exists (Array.exists ((<>) Infinity)) oct)
  let is_bot _ = false
  let top () = Lattice.unsupported "no top element"
  let bot () = Lattice.unsupported "no bot element"

  let print_octagon oct =
    Array.iter (fun a -> Array.iter (fun el -> (match el with
        | Infinity -> print_string "Inf"
        | Val el -> print_float el) ; print_string "\t") a; print_newline ()) oct

  let set_constraint oct const =
    let oct = copy_oct oct in
    (match const with
     | Some (sign1, v1), sign2, v2, upper, Val c ->
       let sign1, sign2, c =
         if not upper
         then not sign1, not sign2, -.c
         else sign1, sign2, c
       in
       let i = v1 * 2 in
       let j = v2 * 2 in
       let i1, j1, i2, j2 =
         (match sign1, sign2 with
          | false, true -> i, j, inverse_index j, inverse_index i
          | true, false -> j, i, inverse_index i, inverse_index j
          | false, false -> i, inverse_index j, j, inverse_index i
          | true, true -> inverse_index j, i, inverse_index i, j)
       in
       set_elt oct i1 j1 (Val c); set_elt oct i2 j2 (Val c)
     | None, sign, v, upper, Val c ->
       let i = v * 2 in
       let c = if upper then c else -.c in
       if upper <> sign
       then set_elt oct i (inverse_index i) (Val (2.0 *. c))
       else set_elt oct (inverse_index i) i (Val (2.0 *. c))
     | _ -> ());
    oct

  let set_var_bounds oct i (lower, upper) =
    (* TODO: don't copy twice *)
    let rec clear j oct =
      if j >= size oct then ()
      else if i = j then clear (j + 1) oct
      else
        let wipe i j =
          let i = 2 * i in
          let j = 2 * j in
          set_elt oct i j Infinity;
          set_elt oct (inverse_index i) j Infinity;
          set_elt oct i (inverse_index j) Infinity;
          set_elt oct (inverse_index i) (inverse_index j) Infinity
        in wipe i j; wipe j i; clear (j + 1) oct
    in
    if i = 1 then ();
    let () = clear 0 oct in
    let oct = set_constraint oct (None, true, i, true, upper) in
    let oct = set_constraint oct (None, true, i, false, lower) in
    oct

  let projection oct i =
    if i < size oct then
      let lower = (get_elt oct (2 * i) (2 * i + 1)) in
      let upper = (get_elt oct (2 * i + 1) (2 * i)) in
      let fn v is_lower = match v with
        | Infinity -> Infinity
        | Val v -> Val ((if is_lower then -. 1.0 else 1.0) *. v /. 2.0)
      in
      (fn lower true, fn upper false)
    else (Val 0.0, Val 0.0)

  let adjust_variable oct k c =
    oct_mapi (fun i j elt ->
        match get_elt oct i j with
        | Infinity -> Infinity
        | Val old ->
          let factor =
            (if j = 2 * k then 1
             else if j = 2 * k + 1 then -1
             else 0)
            +
            (if i = 2 * k then -1
             else if i = 2 * k + 1 then 1
             else 0)
          in
          Val (old +. (float_of_int factor) *. c)
      ) oct

  let adjust_variables oct k l c =
    oct_mapi (fun i j elt ->
        if (j = 2*k && i = 2*l) || (j = 2*l+1 && i = 2*k+1) then
          Val c
        else if (j = 2*l && i = 2*k) || (j = 2*k+1 && i = 2*l+1) then
          Val (-.c)
        else Infinity) oct

  let constraints oct =
    let rec to_string i =
      if i >= size oct then ""
      else let (lower, upper) = projection oct i in
        Printf.sprintf "v%d ∈ [%s,%s]\n" i (elt_to_string lower)
          (elt_to_string upper) ^ to_string (i+1)
    in to_string 0

  let show x = "array octagon"
end
