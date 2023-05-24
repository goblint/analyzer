open Unix

let iso8601_of_tm {tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
  (* assumes UTC tm (from gmtime) *)
  Printf.sprintf "%04u-%02u-%02uT%02u:%02u:%02uZ" (1900 + tm_year) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let iso8601_now () = iso8601_of_tm (gmtime (time ()))

let seconds_of_duration_string =
  let unit = function
    | "" | "s" -> 1
    | "m" -> 60
    | "h" -> 60 * 60
    | s -> invalid_arg ("Unkown duration unit " ^ s ^ ". Supported units are h, m, s.")
  in
  let int_rest f s = Scanf.sscanf s "%u%s" f in
  let split s = BatString.(head s 1, tail s 1) in
  let rec f i s =
    let u, r = split s in (* unit, rest *)
    i * (unit u) + if r = "" then 0 else int_rest f r
  in
  int_rest f
