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

let find_copyof = CopyOfHashTable.find_opt copyof

let rec find_original s =
  match find_copyof s with
  | None -> s
  | Some s' -> (find_original [@tailcall]) s'
