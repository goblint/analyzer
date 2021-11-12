open Prelude

let basename = "compile_commands.json"

type command_object = {
  directory: string;
  file: string;
  command: string option;
  arguments: string list option;
  output: string option;
} [@@deriving yojson]

type t = command_object list [@@deriving yojson]

let parse_file filename =
  Result.get_ok (of_yojson (Yojson.Safe.from_file filename))

let preprocess ~include_args filename =
  let cd = Yojson.Safe.from_file filename in
  let open Yojson.Safe.Util in
  let i = ref 0 in
  convert_each (fun entry ->
      let file = entry |> member "file" |> to_string in
      (* let command = entry |> member "command" |> to_string in *)
      let arguments = entry |> member "arguments" |> convert_each to_string in
      let o_re = Str.regexp "-o +[^ ]+" in (* TODO: include args *)
      let file' = Printf.sprintf "%d.i" !i in
      (* let command' = Str.replace_first o_re (cppflags ^ " -E -o " ^ file') command in *)

      let (o_i, _) = List.findi (fun i e -> e = "-o") arguments in
      let (arguments_init, _ :: _ :: arguments_tl) = List.split_at o_i arguments in
      let arguments' = arguments_init @ include_args @ "-E" :: "-o" :: file' :: arguments_tl in (* TODO: custom includes, cppflags *)
      let command' = Filename.quote_command (List.hd arguments') (List.tl arguments') in

      Printf.printf "CD: %s: %s\n" file command';
      ignore (Sys.command command');
      incr i;
      file'
    ) cd
