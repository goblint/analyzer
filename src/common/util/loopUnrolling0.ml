open GoblintCil

module CopyOfHashTable = Hashtbl.Make(struct
    type t = stmt
    (* Identity by physical equality. *)
    let equal = (==)
    (* Hash only labels and skind (statement itself) because they should remain unchanged between now
       and lookup after analysis.
       CFG construction modifies sid, succs, preds and fallthrough, which are empty here.*)
    let hash (s: stmt) = Hashtbl.hash (s.skind, s.labels)
  end)
let copyof = CopyOfHashTable.create 113

let rec find_original s =
  (* TODO: actually need to recursively follow copies in case of nested unrolled loops? kind of like union-find *)
  match CopyOfHashTable.find_opt copyof s with
  | None -> s
  | Some s' -> (find_original [@tailcall]) s'
