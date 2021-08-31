
let pp_set_ansi_color_tags ppf =
  let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
  let stag_functions' = {stag_functions with
      mark_open_stag = (function
        (* TODO: support all colors like colorize *)
        | Format.String_tag "red" -> Format.sprintf "\027[%sm" "0;31"
        | Format.String_tag "yellow" -> Format.sprintf "\027[%sm" "0;33"
        | Format.String_tag "blue" -> Format.sprintf "\027[%sm" "0;34"
        | Format.String_tag "white" -> Format.sprintf "\027[%sm" "0;37"
        | Format.String_tag "green" -> Format.sprintf "\027[%sm" "0;32"
        | Format.String_tag "violet" -> Format.sprintf "\027[%sm" "0;35"
        | Format.String_tag s -> Format.sprintf "{%s}" s
        | _ -> "");
      mark_close_stag = (function
        | Format.String_tag _ -> "\027[0m"
        | _ -> "");
    }
  in
  Format.pp_set_formatter_stag_functions ppf stag_functions';
  Format.pp_set_mark_tags ppf true
