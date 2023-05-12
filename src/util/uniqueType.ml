open GoblintCil

(* Type invariant variables. *)
let type_inv_tbl = Hashtbl.create 13
let type_inv (c:compinfo) : varinfo =
  try Hashtbl.find type_inv_tbl c.ckey
  with Not_found ->
    let i = Cilfacade.create_var (makeGlobalVar ("{struct "^c.cname^"}") (TComp (c,[]))) in
    Hashtbl.add type_inv_tbl c.ckey i;
    i

let find (t:typ): varinfo option =
  let me_gusta x = List.mem x (GobConfig.get_string_list "exp.unique") in
  match unrollType t with
  | TComp (ci,_) when me_gusta ci.cname -> Some (type_inv ci)
  | _ -> (None : varinfo option)
