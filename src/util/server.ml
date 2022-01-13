open Batteries
open Jsonrpc

type t = {
  file: Cil.file;
  do_analyze: Analyses.increment_data -> Cil.file -> unit;
}

module type Request = sig
  val name: string

  type params
  type response

  val params_of_yojson: Yojson.Safe.t -> (params, string) result
  val response_to_yojson: response -> Yojson.Safe.t

  val process: params -> t -> response
end

module Registry = struct
  type t = (string, (module Request)) Hashtbl.t
  let make () : t = Hashtbl.create 32
  let register (reg: t) (module R : Request) = Hashtbl.add reg R.name (module R)
end

let registry = Registry.make ()

let handle_exn id exn =
  Response.Error.(make Code.InternalError (Printexc.to_string exn) () |> Response.error id)

module ParamParser (R : Request) = struct
  let parse params =
    let maybe_params =
      params
      |> Option.map_default Message.Structured.to_json `Null
      |> R.params_of_yojson
    in
    match maybe_params with
    | Ok params -> Ok params
    | Error err ->
      (* This is a hack to handle cases where R.params is a primitive type like int or string. *)
      match params with
      | Some `List [param] -> R.params_of_yojson param |> Result.map_error (fun _ -> err)
      | _ -> Error err
end

let handle_request (serv: t) (message: Message.either) (id: Id.t) =
  let req = Hashtbl.find_option registry message.method_ in
  let response = match req with
    | Some (module R) ->
      let module Parser = ParamParser (R) in (
        match Parser.parse message.params with
        | Ok params -> (
            try
              R.process params serv
              |> R.response_to_yojson
              |> Response.ok id
            with exn -> handle_exn id exn)
        | Error s -> Response.Error.(make Code.InvalidParams s () |> Response.error id))
    | _ -> Response.Error.(make Code.MethodNotFound message.method_ () |> Response.error id)
  in
  Response.yojson_of_t response |> Yojson.Safe.to_string |> print_endline

let serve serv =
  stdin
  |> IO.to_input_channel
  |> Yojson.Safe.linestream_from_channel
  |> Stream.iter (fun line ->
      match line with
      | `Json json -> (
          try
            let message = Message.either_of_yojson json in
            match message.id with
            | Some id -> handle_request serv message id
            | _ -> () (* We just ignore notifications for now. *)
          with exn -> prerr_endline (Printexc.to_string exn))
      | `Exn exn -> prerr_endline (Printexc.to_string exn))

let make file do_analyze : t = { file; do_analyze }

let start file do_analyze =
  GobConfig.set_bool "incremental.save" true;
  serve (make file do_analyze)

let analyze ?(reset=false) { file; do_analyze } =
  if reset then (
    Serialize.server_solver_data := None;
    Messages.Table.(MH.clear messages_table);
    Messages.Table.messages_list := []);
  let increment_data, fresh = match !Serialize.server_solver_data with
    | Some solver_data ->
      let changes = CompareCIL.compareCilFiles file file in
      let old_data = Some { Analyses.cil_file = file; solver_data } in
      { Analyses.changes; old_data; new_file = file }, false
    | _ -> Analyses.empty_increment_data file, true
  in
  GobConfig.set_bool "incremental.load" (not fresh);
  do_analyze increment_data file;
  GobConfig.set_bool "incremental.load" true

let () =
  let register = Registry.register registry in

  register (module struct
    let name = "analyze"
    type params = { reset: bool [@default false] } [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process { reset } serve = analyze serve ~reset
  end);

  register (module struct
    let name = "config"
    type params = string * Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process (conf, json) _ = GobConfig.set_auto conf (Yojson.Safe.to_string json)
  end);

  register (module struct
    let name = "merge_config"
    type params = Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process json _ = GobConfig.merge json
  end);

  register (module struct
    let name = "messages"
    type params = unit [@@deriving of_yojson]
    type response = Messages.Message.t list [@@deriving to_yojson]
    let process () _ = !Messages.Table.messages_list
  end);

  register (module struct
    let name = "exp_eval"
    type params = ExpressionEvaluation.query [@@deriving of_yojson]
    type response =
      ((string * CilType.Location.t * string * int) * bool option) list [@@deriving to_yojson]
    let process query serv =
      GobConfig.set_auto "trans.activated[+]" "'expeval'";
      ExpressionEvaluation.gv_query := Some query;
      analyze serv;
      GobConfig.set_auto "trans.activated[-]" "'expeval'";
      !ExpressionEvaluation.gv_results
  end);

  register (module struct
    let name = "ping"
    type params = unit [@@deriving of_yojson]
    type response = [`Pong] [@@deriving to_yojson]
    let process () _ = `Pong
  end)
