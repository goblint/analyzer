module MS = Set.Make (Messages.Message)

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
  GobFormat.pp_set_ansi_color_tags Format.std_formatter;

  let left_messages = load_messages Sys.argv.(1) in
  let right_messages = load_messages Sys.argv.(2) in
  let left_only_messages = MS.diff left_messages right_messages in
  let right_only_messages = MS.diff right_messages left_messages in

  if not (MS.is_empty left_only_messages) then (
    Printf.printf "Left-only messages (%d):\n" (MS.cardinal left_only_messages);
    MS.iter (Messages.print) left_only_messages;
  );
  print_newline ();
  if not (MS.is_empty right_only_messages) then (
    Printf.printf "Right-only messages (%d):\n" (MS.cardinal right_only_messages);
    MS.iter (Messages.print) right_only_messages;
  )
