open LibraryDesc

module Pattern =
struct
  type ('a, 'k, 'r) t = 'a -> 'k -> 'r

  exception Expected of string
  let fail s = raise (Expected s)

  let (__): _ t = fun x k -> k x
  let drop: _ t = fun _ k -> k

  let nil: _ t = fun x k ->
    match x with
    | [] -> k
    | _ -> fail "nil"

  let ( ^:: ) (p1: _ t) (p2: _ t): _ t = fun x k ->
    match x with
    | x1 :: x2 ->
      let k = p1 x1 k in
      let k = p2 x2 k in
      k
    | [] -> fail "^::"

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
  match_arg: (Cil.exp, 'k, 'r) Pattern.t;
  match_var_args: (Cil.exp list, 'l, 'r) Pattern.t;
}

type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc
  | VarArgs: (_, 'l, 'r) arg_desc -> ('l, 'r) args_desc
  | (::): ('k, _, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc

let rec match_args: type k r. (k, r) args_desc -> (Cil.exp list, k, r) Pattern.t = function
  | [] -> Pattern.nil
  | VarArgs {match_var_args; _} -> match_var_args
  | {match_arg; _} :: args -> Pattern.(match_arg ^:: match_args args)


let rec accs: type k r. (k, r) args_desc -> accs = fun args_desc args ->
  match args_desc, args with
  | [], [] -> []
  | VarArgs arg_desc, args ->
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

let special ?(attrs:attr list=[]) args_desc special_cont = {
  special = Fun.flip (match_args args_desc) special_cont;
  accs = accs args_desc;
  attrs;
}

let unknown ?attrs args_desc = special ?attrs args_desc `Unknown

let empty____desc = {
  match_arg = Pattern.(__);
  match_var_args = Pattern.(__);
  accesses = [];
}
let __ (_name: string) accesses = { empty____desc with accesses; }
let __' accesses = { empty____desc with accesses; }

let empty_drop_desc = {
  match_arg = Pattern.drop;
  match_var_args = Pattern.drop;
  accesses = [];
}
let drop (_name: string) accesses = { empty_drop_desc with accesses; }
let drop' accesses = { empty_drop_desc with accesses; }


let r = `Read
let w = `Write
let f = `Free
