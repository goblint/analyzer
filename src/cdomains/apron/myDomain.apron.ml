open Prelude
open Pretty
open Cil

module MyD2: RelationDomain.RelD2 =
struct

  include EnvDomain.EnvOps

  type var = EnvDomain.Var.t
  type t = {
    mutable d : (((var, (int) Array.t) BatHashtbl.t) * ((int) Array.t)) Option.t;
    mutable env : Apron.Environment.t
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
          | Some (hst, c) -> Some (hst, c))
  in {d = d'; env = env'}

  let remove_vars a vars =
    let vs' = get_filtered_vars (a.env) vars in
      let env' = Apron.Environment.add a.env vs' [||] in
        let d' = (match a.d with
          | None -> None
          | Some (hst, c) -> Some (hst, c))
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