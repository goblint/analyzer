
let load_messages filename =
  Yojson.Safe.from_file filename
  |> Yojson.Safe.Util.member "messages"
  |> [%of_yojson: Messages.Message.t list]
  |> function
  | Ok l -> l
  | Error s -> failwith s

let () =
  let messages = load_messages Sys.argv.(1) in
  List.iter (fun message ->
      Messages.print ~ppf:Format.std_formatter message
    ) messages
