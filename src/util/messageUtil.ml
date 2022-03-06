open GobConfig

let ansi_color_table =
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
                ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let modes = [(Fun.id, "0" (* normal *)); (String.uppercase_ascii, "1" (* bold *))] in
  colors
  |> List.concat_map (fun (color, color_code) ->
      List.map (fun (mode_fn, mode_code) ->
          (mode_fn color, Format.sprintf "\027[%s;%sm" mode_code color_code)
        ) modes
    )

let colors_on fd = (* use colors? *)
  let c = get_string "colors" in
  c = "always" || c = "auto" && Unix.(isatty fd)

let colorize ~fd msg =
  let on = colors_on fd in
  let replace (color,code) =
    Str.global_replace (Str.regexp ("{"^color^"}")) (if on then code else "")
  in
  let msg = List.fold_right replace ansi_color_table msg in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)
