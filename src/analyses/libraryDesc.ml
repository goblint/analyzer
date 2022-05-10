module Access =
struct
  type kind =
    | Read
    | Write
    | Free

  type t = {
    kind: kind;
    deep: bool;
  }
end

type special =
  | Malloc of Cil.exp
  | Calloc of { count: Cil.exp; size: Cil.exp; }
  | Realloc of { ptr: Cil.exp; size: Cil.exp; }
  | Assert of Cil.exp
  | Lock of { lock: Cil.exp; try_: bool; write: bool; return_on_success: bool; }
  | Unlock of Cil.exp
  | ThreadCreate of { thread: Cil.exp; start_routine: Cil.exp; arg: Cil.exp; }
  | ThreadJoin of { thread: Cil.exp; ret_var: Cil.exp; }
  | Unknown (* TODO: rename to Other? *)
  | Memset of { dest: Cil.exp; ch: Cil.exp; count: Cil.exp; }
  | Bzero of { dest: Cil.exp; count: Cil.exp; }


module Accesses =
struct
  type t = Cil.exp list -> (Access.t * Cil.exp list) list

  (* TODO: remove after migration *)
  type old = [`Read | `Write ] -> Cil.exp list -> Cil.exp list
  let of_old (f: old): t = fun args ->
    [
      ({ kind = Read; deep = true; }, f `Read args);
      ({ kind = Write; deep = true; }, f `Write args);
      ({ kind = Free; deep = true; }, f `Write args); (* old write also imply free *) (* TODO: change after interactive *)
    ]

  (* TODO: remove/rename after migration? *)
  let old (accs: t): Access.t -> Cil.exp list -> Cil.exp list = fun acc args ->
    BatOption.(List.assoc_opt acc (accs args) |? [])

  let old' (accs: t): [`Read | `Write] -> Cil.exp list -> Cil.exp list = fun acc args ->
    let o a = old accs a args in
    match acc with
    | `Read -> o { kind = Read; deep = true; } @ o { kind = Read; deep = false; } @ o { kind = Write; deep = true; } @ o { kind = Write; deep = false; } @ o { kind = Free; deep = true; } @ o { kind = Free; deep = false; }
    | `Write -> o { kind = Write; deep = true; } @ o { kind = Write; deep = false; } @ o { kind = Free; deep = true; } @ o { kind = Free; deep = false; }

  let iter (accs: t) (f: Access.t -> Cil.exp -> unit) args: unit =
    accs args
    |> List.iter (fun (acc, exps) ->
        List.iter (fun exp -> f acc exp) exps
      )

  let fold (accs: t) (f: Access.t -> Cil.exp -> 'a -> 'a) args (a: 'a): 'a =
    accs args
    |> List.fold_left (fun a (acc, exps) ->
        List.fold_left (fun a exp -> f acc exp a) a exps
      ) a
end

type attr =
  | ThreadUnsafe
  | InvalidateGlobals (* TODO: AccessGlobals of Access.t list? *)

type t = {
  special: Cil.exp list -> special;
  accs: Accesses.t;
  attrs: attr list;
}

let special_of_old classify_name = fun args ->
  match classify_name args with
  | `Malloc e -> Malloc e
  | `Calloc (count, size) -> Calloc { count; size; }
  | `Realloc (ptr, size) -> Realloc { ptr; size; }
  | `Assert e -> Assert e
  | `Lock (try_, write, return_on_success) -> Lock { lock = List.hd args; try_; write; return_on_success; }
  | `Unlock -> Unlock (List.hd args)
  | `ThreadCreate (thread, start_routine, arg) -> ThreadCreate { thread; start_routine; arg; }
  | `ThreadJoin (thread, ret_var) -> ThreadJoin { thread; ret_var; }
  | `Unknown _ -> Unknown

let of_old ?(attrs: attr list=[]) (old_accesses: Accesses.old) (classify_name): t = {
  attrs;
  accs = Accesses.of_old old_accesses;
  special = special_of_old classify_name;
}
