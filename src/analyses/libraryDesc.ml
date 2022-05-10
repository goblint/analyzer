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
  | Lock of Cil.exp
  | ThreadCreate of { thread: Cil.exp; start_routine: Cil.exp; arg: Cil.exp; }
  | Realloc of { ptr: Cil.exp; size: Cil.exp; }
  | Unknown


module Accesses =
struct
  type t = Cil.exp list -> (Access.t * Cil.exp list) list

  let old (accs: t): Cil.exp list -> Access.t -> Cil.exp list = fun args acc ->
    BatOption.(List.assoc_opt acc (accs args) |? [])

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

type t = {
  special: Cil.exp list -> special;
  accs: Accesses.t;
  attrs: attr list;
}
