open Prelude

let basename = "compile_commands.json"

type command_object = {
  directory: string;
  file: string;
  command: string option [@default None];
  arguments: string list option [@default None];
  output: string option [@default None];
} [@@deriving yojson]

type t = command_object list [@@deriving yojson]

let parse_file filename =
  Result.get_ok (of_yojson (Yojson.Safe.from_file filename))

let command_o_regexp = Str.regexp "-o +[^ ]+"

let preprocess ~include_args filename =
  parse_file filename
  |> List.mapi (fun i obj ->
      let file = obj.file in
      let preprocessed_file = Printf.sprintf "%d.i" i in (* TODO: better filenames and parent directory *)
      let preprocess_command = match obj.command, obj.arguments with
        | Some command, None ->
          (* TODO: extract o_file *)
          let preprocess_command = Str.replace_first command_o_regexp (String.join " " include_args ^ " -E -o " ^ preprocessed_file) command (* TODO: cppflags *) in
          if preprocess_command = command then (* easier way to check if match was found (and replaced) *)
            failwith "CompilationDatabase.preprocess: no -o argument found for " ^ file
          else
            preprocess_command
        | None, Some arguments ->
          begin match List.findi (fun i e -> e = "-o") arguments with
            | (o_i, _) ->
              begin match List.split_at o_i arguments with
                | (arguments_init, _ :: o_file :: arguments_tl) ->
                  let preprocess_arguments = arguments_init @ include_args @ "-E" :: "-o" :: preprocessed_file :: arguments_tl in (* TODO: cppflags *)
                  Filename.quote_command (List.hd preprocess_arguments) (List.tl preprocess_arguments)
                | _ ->
                  failwith "CompilationDatabase.preprocess: no -o argument value found for " ^ file
              end
            | exception Not_found ->
              failwith "CompilationDatabase.preprocess: no -o argument found for " ^ file
          end
        | Some _, Some _ ->
          failwith "CompilationDatabase.preprocess: both command and arguments specified for " ^ file
        | None, None ->
          failwith "CompilationDatabase.preprocess: neither command nor arguments specified for " ^ file
      in
      Printf.printf "CD: %s: %s\n" file preprocess_command;
      (* TODO: run command relative to obj.directory *)
      ignore (Sys.command preprocess_command); (* TODO: command failure handling *)
      preprocessed_file
    )
