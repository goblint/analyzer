open LibraryDesc

module Pat =
struct
  type ('a, 'k, 'r) t = 'a -> 'k -> 'r

  let (__): _ t = fun x k -> k x
  let drop: _ t = fun _ k -> k

  let nil: _ t = fun x k ->
    match x with
    | [] -> k
    | _ -> invalid_arg "nil"

  let ( ^:: ) (p1: _ t) (p2: _ t): _ t = fun x k ->
    match x with
    | x1 :: x2 ->
      let k = p1 x1 k in
      let k = p2 x2 k in
      k
    | [] -> invalid_arg "^::"

  let map (p: _ t) ~(f): _ t = fun x k -> p x (f k)
  let map_result (p: _ t) ~(f): _ t = fun x k -> f (p x k)
  let (>>) (p: ('a, 'k, 'r) t) (k: 'k) (a: 'a): 'r = p a k

  let many (p: _ t): _ t = fun l k ->
    let rec aux accu = function
      | [] -> k (List.rev accu)
      | x :: xs -> p x (fun x -> aux (x :: accu) xs)
    in
    aux [] l

  let as__ (p: _ t): _ t = fun x k ->
    let k = p x (k x) in
    k
end

type ('k, 'l, 'r) arg_desc = {
  accesses: access list;
  capture: (Cil.exp, 'k, 'r) Pat.t;
  capture': (Cil.exp list, 'l, 'r) Pat.t;
}

type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc
  | Var: ('k, 'l, 'r) arg_desc -> ('l, 'r) args_desc
  | (::): ('k, 'l, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc

let rec special: type k r. (k, r) args_desc -> (Cil.exp list, k, r) Pat.t = function
  | [] -> Pat.nil
  | Var arg -> arg.capture'
  | arg :: args -> Pat.(^::) arg.capture (special args)


let rec accs: type k r. (k, r) args_desc -> accs = fun args_desc args ->
  match args_desc, args with
  | [], [] -> []
  | Var arg_desc, args ->
    List.map (fun acc ->
        (acc, args)
      ) arg_desc.accesses
  | arg_desc :: args_desc, arg :: args ->
    let accs'' = accs args_desc args in
    List.fold_left (fun (accs'': (access * Cil.exp list) list) (acc: access) ->
        match List.assoc_opt acc accs'' with
        | Some args -> (acc, arg :: args) :: List.remove_assoc acc accs''
        | None -> (acc, arg :: args) :: accs''
      ) accs'' arg_desc.accesses
  | _, _ -> invalid_arg "accs"

let map ?(attrs:attr list=[]) p f = {
  special = Pat.(special p >> f);
  accs = accs p;
  attrs;
}

let unknown ?attrs p = map ?attrs p `Unknown

let (>>) p f = map p f

let r = `Read
let w = `Write
let f = `Free
let (__) = fun name accesses -> {
  accesses;
  capture = Pat.(__);
  capture' = Pat.(__);
}
let drop = fun name accesses -> {
  accesses;
  capture = Pat.drop;
  capture' = Pat.drop;
}
let (__') = fun accesses -> {
  accesses;
  capture = Pat.(__);
  capture' = Pat.(__);
}
let drop' = fun accesses -> {
  accesses;
  capture = Pat.drop;
  capture' = Pat.drop;
}
