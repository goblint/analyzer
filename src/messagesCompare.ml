open Goblint_lib
module MS = Set.Make (Messages.Message)

let colors = ref true
let filenames = ref []

let arg_anon filename =
  filenames := filename :: !filenames

let arg_spec = [
  ("--no-colors", Arg.Clear colors, "Disable colored output");
]

let load_messages filename =
  Yojson.Safe.from_file filename
  |> Yojson.Safe.Util.member "messages"
  |> [%of_yojson: Messages.Message.t list]
  |> begin function
    | Ok l -> l
    | Error s -> failwith s
  end
  |> MS.of_list

let () =
  Arg.parse arg_spec arg_anon "messagesCompare [--no-colors] <leftfile> <rightfile>";
  if !colors then
    GobFormat.pp_set_ansi_color_tags Format.std_formatter;

  match List.rev !filenames with
  | [left_filename; right_filename] ->
    let left_messages = load_messages left_filename in
    let right_messages = load_messages right_filename in
    let left_only_messages = MS.diff left_messages right_messages in
    let right_only_messages = MS.diff right_messages left_messages in

    if not (MS.is_empty left_only_messages) then (
      Logs.info "Left-only messages (%d):" (MS.cardinal left_only_messages);
      MS.iter (Messages.print) left_only_messages;
    );
    Logs.newline ();
    if not (MS.is_empty right_only_messages) then (
      Logs.info "Right-only messages (%d):" (MS.cardinal right_only_messages);
      MS.iter (Messages.print) right_only_messages;
    )
  | _ ->
    failwith "filenames"
