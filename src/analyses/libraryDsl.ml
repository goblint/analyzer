open LibraryDesc

module Pat =
struct
  type ('a, 'k, 'r) t = 'a -> 'k -> 'r

  let arg: _ t = fun x k -> k x
  let ignore: _ t = fun _ k -> k

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
  accesses: access list; (* TODO: set *)
  capture: (Cil.exp, 'k, 'r) Pat.t;
  capture': (Cil.exp list, 'l, 'r) Pat.t;
}

(* type ('k, 'r) arg_desc' =
  | []: ('r, 'r) arg_desc'
  | (::): ('k, 'm) arg_desc * ('m, 'r) arg_desc' -> ('k, 'r) arg_desc' *)


type ('k, 'r) args_desc =
  | []: ('r, 'r) args_desc
  (* | VarArg: (Cil.exp -> 'k, 'r) arg_desc -> (Cil.exp list -> 'r, 'r) args_desc *)
  (* | VarIgnore: ('r, 'r) arg_desc -> ('r, 'r) args_desc *)
  | Var: ('k, 'l, 'r) arg_desc -> ('l, 'r) args_desc
  | (::): ('k, 'l, 'm) arg_desc * ('m, 'r) args_desc -> ('k, 'r) args_desc

(* let (__) = {
  accesses = [];
  capture = Pat.ignore;
}

let r c = {
  accesses = [`Read];
  capture = c
}
let rw c = {
  accesses = [`Read; `Write];
  capture = c
} *)

(* let rec special': type k r. (k, r) arg_desc' -> (Cil.exp, k, r) Pat.t = function
  | [] -> fun x k -> k
  (* | arg :: args -> Pat.(^::) arg.capture (special' args) *)
  | arg :: args -> fun x k ->
    let k = arg.capture x k in
    let k = special' args x k in
    k *)

let rec special: type k r. (k, r) args_desc -> (Cil.exp list, k, r) Pat.t = function
  | [] -> Pat.nil
  (* | VarArg arg -> Pat.many Pat.arg *)
  (* | VarArg arg -> fun xs k -> Pat.many (fun x k -> k x) xs k *)
  (* | VarIgnore arg -> Pat.ignore *)
  (* | VarArg arg -> Pat.many (Pat.as__ arg.capture) *)
  (* | arg :: args -> Pat.(^::) (special' arg) (special args) *)
  | Var arg -> arg.capture'
  | arg :: args -> Pat.(^::) arg.capture (special args)


(* let rec accs': type k r. (k, r) arg_desc' -> access list = function
  | [] -> []
  | arg_desc' :: arg_desc'' ->
    let accs' = accs' arg_desc'' in
    arg_desc'.accesses @ accs' *)

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
      (* ) accs'' (accs' arg_desc) *)
      ) accs'' arg_desc.accesses
  | _, _ -> invalid_arg "accs"

let (>>) p f = {
  special = Pat.(special p >> f);
  accs = accs p;
}

let r = `Read
let w = `Write
let f = `Free
let (__) = fun accesses -> {
  accesses;
  capture = Pat.arg;
  capture' = Pat.arg;
}
let (~~) = fun accesses -> {
  accesses;
  capture = Pat.ignore;
  capture' = Pat.ignore;
}

let (>:) name accesses = {
  accesses;
  capture = Pat.arg;
  capture' = Pat.arg;
}
let (>~) name accesses = {
  accesses;
  capture = Pat.ignore;
  capture' = Pat.ignore;
}

(* let p = [r Pat.arg; rw Pat.ignore; rw Pat.arg] >> fun e1 r2 -> `Lock e1 *)
(* let p = [[r; __]; [r]; [r; w; __]; "foo".%{`Read; `Write}; "foo".%{`Read}] >> fun e1 r2 -> `Lock e1 *)
let p = [__ [r]; ~~ [r; w]; "dest" >: []] >> fun e1 r2 -> `Lock e1
let p = [~~ [r]; ~~ [r; w]; "dest" >~ []] >> `Unknown
let s = p.special
let a = p.accs
