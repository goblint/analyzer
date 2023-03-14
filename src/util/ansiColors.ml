let table =
  let open GobList.Syntax in
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
                ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let modes = [(Fun.id, "0" (* normal *)); (String.uppercase_ascii, "1" (* bold *))] in
  let+ (color, color_code) = colors
  and+ (mode_fn, mode_code) = modes in
  (mode_fn color, Format.sprintf "\027[%s;%sm" mode_code color_code)

let stderr = ref true (* matches schema default *)
