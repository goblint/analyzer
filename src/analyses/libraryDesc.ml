
type access = [
  | `Read
  | `Write
  | `Free
]

type special = [
  | `Lock of Cil.exp
  | `ThreadCreate of (Cil.exp * Cil.exp * Cil.exp)
  | `Realloc of (Cil.exp * Cil.exp)
  | `Unknown
]


type accs = Cil.exp list -> (access * Cil.exp list) list

(* TODO: rename to t *)
type desc = {
  special: Cil.exp list -> special;
  accs: accs;
}
