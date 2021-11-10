open Prelude
open Pretty
open Cil

module Matrix =
struct
  type t = int list list

  let rec add_column (m : t) col pos =
    match m with
    | [] -> if pos > 0 then [] else [col]
    | x :: xs -> if pos > 0 then (x :: (add_column xs col (pos - 1)))
                  else col :: m

  let rec remove_column (m : t) pos =
    match m with
    | [] -> []
    | x :: xs -> if pos > 0 then (x :: (remove_column xs (pos - 1)))
                  else m

  let rec append_zero_row m =
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
end

module MyD2: RelationDomain.RelD2 =
struct

  include EnvDomain.EnvOps

  type var = EnvDomain.Var.t
  type t = {
    d :  Matrix.t Option.t;
    env : Apron.Environment.t
  }

  let is_bot_env a =
    match a.d with
    | None -> true
    | _ -> false

  let equal a b = Apron.Environment.equal (a.env) (b.env) (* ToDo Check for d equality *)
  let hash a = 0
  let compare a b = 0
  let show a = ""
  let pretty () (x:t) = text (show x)
  let printXml a b = ()
  let name () = ""
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

  let add_vars a vars =
    let vs' = get_filtered_vars (a.env) vars in
      let env' = Apron.Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
          | Some (m) -> Some (m))
  in {d = d'; env = env'}

  let remove_vars a vars =
    let vs' = get_filtered_vars (a.env) vars in
      let env' = Apron.Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
          | Some (m) -> Some (m))
  in {d = d'; env = env'}

  let remove_vars_with a b = ()
  let remove_filter_with a f = failwith "unimplemented"

  let keep_filter a f = failwith "unimplemented"
  let forget_vars a l = failwith "unimplemented"

  let assign_exp a b c = failwith "unimplemented"
  let assign_var a b c = failwith "unimplemented"
  let assign_var_parallel_with a b = ()
  let substitute_exp a b c = failwith "unimplemented"
  let type_tracked a = false
  let varinfo_tracked a = false
  let assert_inv a b c = a
  let eval_int a b = failwith "unimplemented"
  let unify a b = a
end