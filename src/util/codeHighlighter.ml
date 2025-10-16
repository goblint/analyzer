type t = Fpath.t -> string BatEnum.t

let none: t = fun file ->
  BatFile.lines_of (Fpath.to_string file)
  |> BatEnum.map XmlUtil.escape  (* nosemgrep: batenum-module *)

let pygments_command = "pygmentize"
let pygments_style = "default"
let pygments_arguments = ["-f"; "html"; "-O"; "nowrap,classprefix=pyg-"]

let make_pygments ~(style_css_file): t option =
  let command = Filename.quote_command pygments_command ("-S" :: pygments_style :: pygments_arguments) ~stdout:(Fpath.to_string style_css_file) in
  match Sys.command command with
  | 0 ->
    let pygments file =
      let command = Filename.quote_command pygments_command (Fpath.to_string file :: pygments_arguments) in
      let ic = Unix.open_process_in command in
      let ic' = BatIO.input_channel ic in
      BatIO.lines_of ic'
    in
    Some pygments
  | _ ->
    None

let make ~(style_css_file): t =
  Option.value (make_pygments ~style_css_file) ~default:none
