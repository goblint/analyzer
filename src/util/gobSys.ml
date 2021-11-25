open Prelude

let rec mkdir_parents filename =
  let dirname = Filename.dirname filename in
  if not (Sys.file_exists dirname) then (
    mkdir_parents dirname;
    Unix.mkdir dirname 0o770; (* TODO: what permissions? *)
  )
