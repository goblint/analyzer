open Batteries
open Jsonrpc

type t = {
  file: Cil.file;
  do_analyze: Analyses.increment_data -> Cil.file -> unit;
}

module type Command = sig
  val name: string

  type args
  type result

  val args_of_yojson: Yojson.Safe.t -> args Ppx_deriving_yojson_runtime.error_or
  val result_to_yojson: result -> Yojson.Safe.t

  val process: args -> t -> result
end

module Registry = struct
  type t = (string, (module Command)) Hashtbl.t
  let make () : t = Hashtbl.create 32
  let register (reg : t) (module R : Command) = Hashtbl.add reg R.name (module R)
end

let registry = Registry.make ()

let handle_exn id exn =
  Response.Error.(make Code.InternalError (Printexc.to_string exn) () |> Response.error id)

let handle_request (serv : t) (message: Message.either) (id: Id.t) =
  let cmd = Hashtbl.find_option registry message.method_ in
  let response = match cmd with
    | Some (module C) ->
      let args = 
        message.params
        |> Option.map_default Message.Structured.to_json `Null
        |> C.args_of_yojson
      in
      (match args with
       | Ok args -> (
           try
             C.process args serv
             |> C.result_to_yojson
             |> Response.ok id
           with exn -> handle_exn id exn)
       | Error s -> Response.Error.(make Code.InvalidParams s () |> Response.error id))
    | _ -> Response.Error.(make Code.MethodNotFound message.method_ () |> Response.error id)
  in
  Response.yojson_of_t response |> Yojson.Safe.to_string |> print_endline

let serve serv =
  let chan = IO.to_input_channel stdin in
  let stream = Yojson.Safe.linestream_from_channel chan in
  while not (Stream.is_empty stream) do
    let line = Stream.next stream in
    match line with
    | `Json json -> (
        try
          let message = Message.either_of_yojson json in
          match message.id with
          | Some id -> handle_request serv message id
          | _ -> () (* We just ignore notifications for now. *)
        with exn -> prerr_endline (Printexc.to_string exn))
    | `Exn exn -> prerr_endline (Printexc.to_string exn)
  done

let make file do_analyze : t = { file; do_analyze }

let start file do_analyze =
  GobConfig.set_bool "incremental.save" true;
  GobConfig.set_bool "incremental.load" true;
  serve (make file do_analyze)

let change_info file = match !Serialize.solver_data with
  | Some solver_data ->
    let changes = CompareCIL.empty_change_info () in
    let old_data = Some { Analyses.cil_file = file; solver_data } in
    { Analyses.changes; old_data; new_file = file }
  | _ -> Analyses.empty_increment_data file  

let () =
  let register = Registry.register registry in

  register (module struct
    let name = "analyze"
    type args = bool * unit [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]
    let process (reset, ()) serve =
      if reset then (
        Serialize.solver_data := None;
        GobConfig.set_bool "incremental.load" false);
      serve.do_analyze (change_info serve.file) serve.file;
      if reset then
        GobConfig.set_bool "incremental.load" true
  end);

  register (module struct
    let name = "config"
    type args = string * Yojson.Safe.t [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]
    let process (conf, json) _ = GobConfig.set_auto conf (Yojson.Safe.to_string json)
  end);

  register (module struct
    let name = "merge_config"
    type args = Yojson.Safe.t * unit [@@deriving of_yojson]
    type result = unit [@@deriving to_yojson]
    let process (json, ()) _ = GobConfig.merge json
  end);

  register (module struct
    let name = "messages"
    type args = unit [@@deriving of_yojson]
    type result = Messages.Message.t list [@@deriving to_yojson]
    let process () _ = !Messages.Table.messages_list
  end);

  register (module struct
    let name = "ping"
    type args = unit [@@deriving of_yojson]
    type result = [`Pong] [@@deriving to_yojson]
    let process () _ = `Pong
  end)
