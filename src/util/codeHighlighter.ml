type t = Fpath.t -> string BatEnum.t

let none: t = fun file ->
  BatFile.lines_of (Fpath.to_string file)
  |> BatEnum.map XmlUtil.escape

let make_pygments ~(style_css_file): t option =
  let asd = BatSys.command (GobFormat.asprintf {a|pygmentize -S default -f html -O nowrap,classprefix=pyg- > %a|a} Fpath.pp style_css_file) in
  assert (asd = 0);

  let pygments file =
    let ic = BatUnix.open_process_args_in "pygmentize" [|"pygmentize"; "-f"; "html"; "-O"; "nowrap,classprefix=pyg-"; Fpath.to_string file|] in (* TODO: close *)
    let ic' = BatIO.input_channel ic in
    BatIO.lines_of ic'
  in
  Some pygments

let make ~(style_css_file): t =
  Option.value (make_pygments ~style_css_file) ~default:none
