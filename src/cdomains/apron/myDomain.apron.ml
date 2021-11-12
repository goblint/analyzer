open Prelude
open Pretty
open Cil

module Matrix =
struct
  type t = int list list

  let rec add_column (m : t) col pos : t =
    match m with
    | [] -> if pos > 0 then [] else [col]
    | x :: xs -> if pos > 0 then (x :: (add_column xs col (pos - 1)))
                  else col :: m

  let rec remove_column (m : t) pos =
    match m with
    | [] -> []
    | x :: xs -> if pos > 0 then (x :: (remove_column xs (pos - 1)))
                  else m

  let rec append_zero_row (m :t) :t =
    match m with
    | [] -> []
    | x :: xs -> (List.append x [0]) :: (append_zero_row xs)

  let dim_x (m : t) = List.length m

  let dim_y (m : t) =
    match m with
    | [] -> 0
    | x :: _ -> List.length x

  let create_zero_col (m : t) =
    let rec create_zero_list l n =
      if n > 0 then create_zero_list (List.append [0] l) (n-1)
      else l
    in
    create_zero_list [] (dim_y m)

  let show (x:t) =
    let rec list_str l =
      match l with
      | [] -> ""
      | x :: xs -> (Format.asprintf "%i " x) ^(list_str xs)
    in
    let str_lists = List.map list_str x in
   let conc_with_sep a b = a ^ "\n" ^ b in
   List.fold_left conc_with_sep "" str_lists


end

module MyD2: RelationDomain.RelD2 with type var = EnvDomain.Var.t =
struct

  include EnvDomain.EnvOps
  open Apron

  type var = EnvDomain.Var.t
  type t = {
    d :  Matrix.t Option.t;
    env : Environment.t
  }

  let is_bot_env a =
    match a.d with
    | None -> true
    | _ -> false

  let equal a b = Environment.equal (a.env) (b.env) (* ToDo Check for d equality *)
  let hash a = 0
  let compare a b = 0
  let show a =
    let d_str = (match a.d with
    | None -> "⟂"
    | Some (m) -> Matrix.show m)
    in
   Format.asprintf "%s (env: %a)" d_str (Environment.print:Format.formatter -> Environment.t -> unit) a.env

  let pretty () (x:t) = text (show x)
  let printXml a b = ()
  let name () = "affeq"
  let to_yojson a = failwith "unimplemented"
  let invariant a b = failwith "unimplemented"
  let arbitrary () = failwith "no arbitrary"
  let leq a b = false
  let join a b = a
  let meet a b = a
  let widen a b = a
  let narrow a b = a
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let bot () = failwith "unimplemented"
  let is_bot a = false
  let top () = failwith "unimplemented"
  let is_top a = false
  let copy a = a
  let vars a = vars a.env

  open Apron.Dim

  let dim_add (ch: Apron.Dim.change) m =
    let rec add_cols pos =
      match pos with
      | [] -> m
      | x :: xs -> Matrix.add_column m (Matrix.create_zero_col m) x
    in
     add_cols (Array.to_list ch.dim)

  let dim_remove (ch: Apron.Dim.change) m =
    let rec remove_cols pos =
      match pos with
      | [] -> m
      | x :: xs -> Matrix.remove_column m  x
    in
     remove_cols (Array.to_list ch.dim)

  let add_vars a vars =
    let vs' = get_filtered_vars (a.env) vars in
      let env' = Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
          | Some (m) -> Some (dim_add (Environment.dimchange a.env env') m))
  in {d = d'; env = env'}

  let remove_vars a vars =
    let vs' = get_filtered_vars (a.env) vars in
      let env' = Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
          | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
  in {d = d'; env = env'}

  let remove_vars a b = a
  let remove_filter a f = a

  let keep_filter a f = failwith "unimplemented"
  let forget_vars a l = failwith "unimplemented"

  let assign_exp a b c = failwith "unimplemented"
  let assign_var a b c = failwith "unimplemented"
  let assign_var_parallel a b = a
  let substitute_exp a b c = failwith "unimplemented"
  let type_tracked a = false
  let varinfo_tracked a = false
  let assert_inv a b c = a
  let eval_int a b = failwith "unimplemented"
  let unify a b = a
end