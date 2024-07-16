open Ppxlib

let location_errorf ~loc =
  Format.ksprintf (fun err ->
      raise (Ocaml_common.Location.Error (Ocaml_common.Location.error ~loc err))
    )

let find_file_path ~loc file_name =
  let dirname = loc.Ocaml_common.Location.loc_start.pos_fname |> Filename.dirname in
  let relative_path = Filename.concat dirname file_name in
  List.find Sys.file_exists [relative_path; file_name]

let get_blob ~loc file_name =
  try
    let file_path = find_file_path ~loc file_name in
    let c = open_in_bin file_path in
    let s = String.init (in_channel_length c) (fun _ -> input_char c) in
    close_in c;
    s
  with _ ->
    location_errorf ~loc "[%%blob] could not find or load file %s" file_name

let expand ~ctxt file_name =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.estring ~loc (get_blob ~loc file_name)

let extension =
  Extension.V3.declare "blob" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension extension

;;
Driver.register_transformation ~rules:[rule] "ppx_blob"