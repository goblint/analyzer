
type access = [
  | `Read
  | `Write
]

type special = [
  | `Lock of Cil.exp
  | `Unknown
]


type accs = Cil.exp list -> (access * Cil.exp list) list

(* TODO: rename to t *)
type desc = {
  special: Cil.exp list -> special;
  accs: accs;
}
