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
  parse_file filename
  |> List.mapi (fun i obj ->
      let file = obj.file in
      let preprocessed_file = Printf.sprintf "%d.i" i in
      let preprocess_command = match obj.command, obj.arguments with
        | Some command, None ->
          let o_re = Str.regexp "-o +[^ ]+" in (* TODO: include args *)
          Str.replace_first o_re (String.join " " include_args ^ " -E -o " ^ preprocessed_file) command
        | None, Some arguments ->
          let (o_i, _) = List.findi (fun i e -> e = "-o") arguments in
          let (arguments_init, _ :: _ :: arguments_tl) = List.split_at o_i arguments in
          let arguments' = arguments_init @ include_args @ "-E" :: "-o" :: preprocessed_file :: arguments_tl in (* TODO: custom includes, cppflags *)
          Filename.quote_command (List.hd arguments') (List.tl arguments')
        | Some _, Some _ ->
          failwith "CompilationDatabase.preprocess: both command and arguments specified for " ^ file
        | None, None ->
          failwith "CompilationDatabase.preprocess: neither command nor arguments specified for " ^ file
      in
      Printf.printf "CD: %s: %s\n" file preprocess_command;
      ignore (Sys.command preprocess_command);
      preprocessed_file
    )
