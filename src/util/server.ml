open Batteries
open Jsonrpc

exception Failure of Response.Error.Code.t * string

type t = {
  mutable file: Cil.file;
  mutable max_ids: MaxIdUtil.max_ids;
  input: IO.input;
  output: unit IO.output;
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
  Response.Error.(make ~code:Code.InternalError ~message:(Printexc.to_string exn) () |> Response.error id)

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
            with Failure (code, message) -> Response.Error.(make ~code ~message () |> Response.error id))
        | Error message -> Response.Error.(make ~code:Code.InvalidParams ~message () |> Response.error id))
    | _ -> Response.Error.(make ~code:Code.MethodNotFound ~message:message.method_ () |> Response.error id)
  in
  Response.yojson_of_t response |> Yojson.Safe.to_string |> IO.write_line serv.output;
  IO.flush serv.output

let serve serv =
  serv.input
  |> Lexing.from_channel
  |> GobYojson.seq_from_lexbuf (Yojson.init_lexer ())
  |> Seq.iter (fun json ->
      let message = Message.either_of_yojson json in
      match message.id with
      | Some id -> handle_request serv message id
      | _ -> () (* We just ignore notifications for now. *)
    )

let make ?(input=stdin) ?(output=stdout) file : t =
  let max_ids = MaxIdUtil.get_file_max_ids file in
  {
    file;
    max_ids;
    input;
    output
  }

let bind () =
  let mode = GobConfig.get_string "server.mode" in
  if mode = "stdio" then None, None else (
    let path = GobConfig.get_string "server.unix-socket" in
    if Sys.file_exists path then
      Sys.remove path;
    let socket = Unix.socket PF_UNIX SOCK_STREAM 0 in
    Unix.bind socket (ADDR_UNIX path);
    Unix.listen socket 1;
    let conn, _ = Unix.accept socket in
    Unix.close socket;
    Sys.remove path;
    Some (Unix.input_of_descr conn), Some (Unix.output_of_descr conn))

let start file =
  let input, output = bind () in
  GobConfig.set_bool "incremental.save" true;
  serve (make file ?input ?output)

let reparse (s: t) =
  if GobConfig.get_bool "server.reparse" then (
    GoblintDir.init ();
    Fun.protect ~finally:GoblintDir.finalize Maingoblint.preprocess_and_merge, true)
  else s.file, false

(* Only called when the file has not been reparsed, so we can skip the expensive CFG comparison. *)
let virtual_changes file =
  let eq (glob: Cil.global) _ _ = match glob with
    | GFun (fdec, _) -> CompareCIL.should_reanalyze fdec, false, None
    | _ -> false, false, None
  in
  CompareCIL.compareCilFiles ~eq file file

let increment_data (s: t) file reparsed = match !Serialize.server_solver_data with
  | Some solver_data when reparsed ->
    let changes = CompareCIL.compareCilFiles s.file file in
    let old_data = Some { Analyses.cil_file = s.file; solver_data } in
    s.max_ids <- UpdateCil.update_ids s.file s.max_ids file changes;
    { Analyses.changes; old_data; new_file = file }, false
  | Some solver_data ->
    let changes = virtual_changes file in
    let old_data = Some { Analyses.cil_file = file; solver_data } in
    { Analyses.changes; old_data; new_file = file }, false
  | _ -> Analyses.empty_increment_data file, true

let analyze ?(reset=false) (s: t) =
  Messages.Table.(MH.clear messages_table);
  Messages.Table.messages_list := [];
  let file, reparsed = reparse s in
  if reset then (
    let max_ids = MaxIdUtil.get_file_max_ids file in
    s.max_ids <- max_ids;
    Serialize.server_solver_data := None;
    Serialize.server_analysis_data := None);
  let increment_data, fresh = increment_data s file reparsed in
  Cilfacade.reset_lazy ();
  WideningThresholds.reset_lazy ();
  IntDomain.reset_lazy ();
  ApronDomain.reset_lazy ();
  Access.reset ();
  s.file <- file;
  GobConfig.set_bool "incremental.load" (not fresh);
  Fun.protect ~finally:(fun () ->
      GobConfig.set_bool "incremental.load" true
    ) (fun () ->
      Maingoblint.do_analyze increment_data s.file
    )

let () =
  let register = Registry.register registry in

  register (module struct
    let name = "analyze"
    type params = { reset: bool [@default false] } [@@deriving of_yojson]
    (* TODO: Return analysis results as JSON. Useful for GobPie. *)
    type status = Success | VerifyError | Aborted [@@deriving to_yojson]
    type response = { status: status } [@@deriving to_yojson]
    (* TODO: Add options to control the analysis precision/context for specific functions. *)
    (* TODO: Add option to mark functions as modified. *)
    let process { reset } serve =
      try
        analyze serve ~reset;
        {status = if !Goblintutil.verified = Some false then VerifyError else Success}
      with Sys.Break ->
        assert (GobConfig.get_bool "ana.opt.hashcons"); (* TODO: TD3 doesn't copy input solver data, so will modify it in place and screw up Serialize.server_solver_data accidentally *)
        {status = Aborted}
  end);

  register (module struct
    let name = "config"
    type params = string * Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    (* TODO: Make it possible to change the non-optional parameters. (i.e., the set of input files) *)
    (* TODO: Check options for compatibility with the incremental analysis. *)
    let process (conf, json) _ =
      try
        GobConfig.set_auto conf (Yojson.Safe.to_string json)
      with exn -> raise (Failure (InvalidParams, Printexc.to_string exn))
  end);

  register (module struct
    let name = "merge_config"
    type params = Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process json _ =
      try GobConfig.merge json with exn -> (* TODO: Be more specific in what we catch. *)
        raise (Failure (InvalidParams, Printexc.to_string exn))
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
