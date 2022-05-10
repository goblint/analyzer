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


type accs = Cil.exp list -> (Access.t * Cil.exp list) list

type attr =
  | ThreadUnsafe

type t = {
  special: Cil.exp list -> special;
  accs: accs;
  attrs: attr list;
}
