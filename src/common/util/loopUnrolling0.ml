open GoblintCil

module CopyOfHashTable = Hashtbl.Make(struct
    type t = stmt
    (* Identity by physical equality. *)
    let equal = (==)
    (* Only index statements after CFG numbering. Rebuild after any SID changes. *)
    let hash (s: stmt) = Hashtbl.hash s.sid
  end)
let copyof = CopyOfHashTable.create 113

let reset_copyof () =
  CopyOfHashTable.clear copyof

let rebuild_copyof relationships =
  CopyOfHashTable.clear copyof;
  List.iter (fun (copy, original) -> CopyOfHashTable.replace copyof copy original) relationships

let find_copyof = CopyOfHashTable.find_opt copyof

let rec find_original s =
  match find_copyof s with
  | None -> s
  | Some s' -> (find_original [@tailcall]) s'
