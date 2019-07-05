open Unix

let iso8601_of_tm {tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec} =
  (* assumes UTC tm (from gmtime) *)
  Printf.sprintf "%04u-%02u-%02uT%02u:%02u:%02u" (1900 + tm_year) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let iso8601_now () = iso8601_of_tm (gmtime (time ()))