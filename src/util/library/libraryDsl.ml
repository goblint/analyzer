open LibraryDesc
module Access = LibraryDesc.Access (* avoid spurious dependency cycle due to ocamldep overapprox ambiguity between Access and LibraryDesc.Access *)

(** First-class patterns for arguments matching.
    @see <https://github.com/ocaml-ppx/ppxlib/blob/main/src/ast_pattern.ml> for inspiration from ppxlib. *)
module Pattern =
struct
  (** @param a Type of value to match.
      @param k Type of continuation function.
      @param r Return type of match. *)
  type ('a, 'k, 'r) t = 'a -> 'k -> 'r

  exception Expected of string
  let fail s = raise (Expected s)

  let __: _ t = fun x k -> k x
  let drop: _ t = fun _ k -> k

  let nil: _ t = fun x k ->
    match x with
    | [] -> k
    | _ -> fail "Library function is called with more arguments than expected."

  let ( ^:: ) (p1: _ t) (p2: _ t): _ t = fun x k ->
    match x with
    | x1 :: x2 ->
      let k = p1 x1 k in
      let k = p2 x2 k in
      k
    | [] -> fail "^::"
end

type access =
  | Access of LibraryDesc.Access.t
  | If of (unit -> bool) * access

let rec eval_access = function
  | Access acc -> Some acc
  | If (p, access) ->
    if p () then
      eval_access access
    else
      None

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


let rec accs: type k r. (k, r) args_desc -> Accesses.t = fun args_desc args ->
  match args_desc, args with
  | [], [] -> []
  | VarArgs arg_desc, args ->
    List.filter_map (fun access ->
        match eval_access access with
        | Some acc -> Some (acc, args)
        | None -> None
      ) arg_desc.accesses
  | arg_desc :: args_desc, arg :: args ->
    let accs'' = accs args_desc args in
    List.fold_left (fun (accs'': (Access.t * Cil.exp list) list) (access: access) ->
        match eval_access access with
        | Some acc ->
          begin match List.assoc_opt acc accs'' with
            | Some args -> (acc, arg :: args) :: List.remove_assoc acc accs''
            | None -> (acc, [arg]) :: accs''
          end
        | None -> accs''
      ) accs'' arg_desc.accesses
  | _, _ -> invalid_arg "accs"

let special ?(attrs:attr list=[]) args_desc special_cont = {
  special = Fun.flip (match_args args_desc) special_cont;
  accs = accs args_desc;
  attrs;
}

let special' ?(attrs:attr list=[]) args_desc special_cont = {
  special = (fun args -> Fun.flip (match_args args_desc) (special_cont ()) args); (* eta-expanded such that special_cont is re-executed on each call instead of once during LibraryFunctions construction *)
  accs = accs args_desc;
  attrs;
}

let unknown ?attrs args_desc = special ?attrs args_desc Unknown

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


let r = Access { kind = Read; deep = false; }
let r_deep = Access { kind = Read; deep = true; }
let w = Access { kind = Write; deep = false; }
let w_deep = Access { kind = Write; deep = true; }
let f = Access { kind = Free; deep = false; }
let f_deep = Access { kind = Free; deep = true; }
let s = Access { kind = Spawn; deep = false; }
let s_deep = Access { kind = Spawn; deep = true; }
let c = Access { kind = Spawn; deep = false; } (* TODO: Sound, but very imprecise hack for calls to function pointers given as arguments. *)
let c_deep = Access { kind = Spawn; deep = true; }

let if_ p access = If (p, access)
