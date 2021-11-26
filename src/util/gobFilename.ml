open Prelude

(** Make path absolute, if relative. *)
let absolute filename =
  if Filename.is_relative filename then
    Filename.concat (Unix.getcwd ()) filename
  else
    filename

let chop_prefix ~prefix filename =
  let prefix =
    if String.ends_with prefix Filename.dir_sep then
      prefix
    else
      prefix ^ Filename.dir_sep
  in
  if String.starts_with filename prefix then
    String.lchop ~n:(String.length prefix) filename
  else
    raise (Invalid_argument "GobFilename.chop_prefix: doesn't start with given prefix")

let common_prefix filename1 filename2 =
  let common_prefix_length s1 s2 =
    let l1 = String.length s1 in
    let l2 = String.length s2 in
    let rec common_prefix_length' i =
      if i >= l1 || i >= l2 then
        i
      else if s1.[i] = s2.[i] then
        common_prefix_length' (i + 1)
      else
        i
    in
    common_prefix_length' 0
  in
  String.left filename1 (common_prefix_length filename1 filename2)

let chop_common_prefix filename1 filename2 =
  let filename2 = absolute filename2 in
  chop_prefix ~prefix:(common_prefix (absolute filename1) filename2) filename2

let chop_common_suffix filename1 filename2 =
  String.rev (chop_common_prefix (String.rev filename1) (String.rev filename2))
