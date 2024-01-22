(** XML utilities. *)

(* XML escape extracted here to avoid dependency cycle:
   CilType -> Goblintutil -> GobConfig -> Tracing -> Node -> CilType *)

let escape (x:string):string =
  (* Safe to escape all these everywhere in XML: https://stackoverflow.com/a/1091953/854540 *)
  Str.global_replace (Str.regexp "&") "&amp;" x |>
  Str.global_replace (Str.regexp "<") "&lt;" |>
  Str.global_replace (Str.regexp ">") "&gt;" |>
  Str.global_replace (Str.regexp "\"") "&quot;" |>
  Str.global_replace (Str.regexp "'") "&apos;" |>
  Str.global_replace (Str.regexp "[\x0b\001\x0c\x0f\x0e\x05]") "" |> (* g2html just cannot handle from some kernel benchmarks, even when escaped... *)
  Str.global_replace (Str.regexp "[\x1b]") "" |> (* g2html cannot handle from chrony *)
  Str.global_replace (Str.regexp "\x00") "\\\\0" (* produces \\0, is needed if an example contains \0 *)
