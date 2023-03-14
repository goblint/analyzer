
let pp_set_ansi_color_tags ppf =
  let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
  let mark_open_stag = function
    | Format.String_tag s ->
      begin match List.assoc_opt s AnsiColors.table with
        | Some code -> code
        | None -> Format.sprintf "{%s}" s
      end
    | _ -> ""
  in
  let reset_code = List.assoc "reset" AnsiColors.table in (* assoc only once *)
  let mark_close_stag = function
    | Format.String_tag _ -> reset_code
    | _ -> ""
  in
  let stag_functions' = {stag_functions with mark_open_stag; mark_close_stag} in
  Format.pp_set_formatter_stag_functions ppf stag_functions';
  Format.pp_set_mark_tags ppf true

let pp_print_nothing ppf () = ()
