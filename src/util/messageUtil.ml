open GobConfig

let colors_on fd = (* use colors? *)
  let c = get_string "colors" in
  c = "always" || c = "auto" && Unix.(isatty fd)

let () = AfterConfig.register (fun () ->
    AnsiColors.stderr := colors_on Unix.stderr;
    if !AnsiColors.stderr then
      GobFormat.pp_set_ansi_color_tags Format.err_formatter
  )

let colorize ~fd msg =
  let on = colors_on fd in
  let replace (color,code) =
    Str.global_replace (Str.regexp ("{"^color^"}")) (if on then code else "")
  in
  let msg = List.fold_right replace AnsiColors.table msg in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)
