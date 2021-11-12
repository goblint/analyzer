open Prelude
open Pretty
open Cil
open Apron

module Matrix =
struct
  type t = int list list

  let empty () = ([] : t)

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

module VarManagement =
struct
  include EnvDomain.EnvOps

  type var = EnvDomain.Var.t
  type t = {
    d :  Matrix.t Option.t;
    env : Environment.t
  }

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
      | x :: xs -> Matrix.remove_column m  x (* TODO Every row that has an entry > 0 in this col should be removed as well!*)
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

  let remove_filter a f =
    let env' = remove_filter_with a.env f in
      let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
  in {d = d'; env = env'}

  let keep_filter a f =
    let env' = keep_filter_with a.env f in
      let d' = (match a.d with
        | None -> None
        | Some (m) -> Some (dim_remove (Environment.dimchange a.env env') m))
  in {d = d'; env = env'}

  let forget_vars a l = (*ToDo Mem_var shouldn't be called*)
    remove_vars a l

  let vars a = vars a.env

  let type_tracked typ =
    isIntegralType typ

  let varinfo_tracked vi =
    (* no vglob check here, because globals are allowed in apron, but just have to be handled separately *)
    type_tracked vi.vtype && not vi.vaddrof
end

module MyD2: RelationDomain.RelD2 with type var = EnvDomain.Var.t =
struct

  include VarManagement
  let show a =
    let d_str = (match a.d with
    | None -> "⟂"
    | Some (m) -> Matrix.show m)
    in
   Format.asprintf "%s (env: %a)" d_str (Environment.print:Format.formatter -> Environment.t -> unit) a.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))

  let equal a b = Environment.equal (a.env) (b.env) (* ToDo Check for d equality *)
  let hash a = 0
  let compare a b = 0
  let name () = "affeq"
  let to_yojson a = failwith "ToDo Implement in future"
  let invariant a b = Invariant.none
  let arbitrary () = failwith "no arbitrary"
  let leq a b = false
  let join a b = a
  let meet a b = a
  let widen a b = a
  let narrow a b = a
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let bot () =
     {d = None; env = Environment.make [||] [||]}
  let is_bot a = a.d == None
  let is_bot_env a =
    match a.d with
    | None -> true
    | _ -> false
  let top () = {d = Some (Matrix.empty ()); env = Environment.make [||] [||] }
  let is_top a = a.d == Some (Matrix.empty ())
  let assign_exp a b c = a
  let assign_var a b c = a
  let assign_var_parallel a b = a
  let substitute_exp a b c = a

  let assert_inv a b c = a
  let eval_int a e =
    let module ID = Queries.ID in
      ID.top ()
  let unify a b = a
end