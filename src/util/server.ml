(** Interactive server mode using JSON-RPC. *)

open Batteries
open Jsonrpc
open GoblintCil

module InvariantParser = WitnessUtil.InvariantParser

module type ArgWrapper =
sig
  module Arg: ArgTools.BiArg
  module Locator: module type of WitnessUtil.Locator (Arg.Node)
  val locator: Locator.t
  val find_node: string -> Arg.Node.t
  val find_cfg_node: string -> Arg.Node.t list
end

type t = {
  mutable file: Cil.file option;
  mutable max_ids: MaxIdUtil.max_ids;
  arg_wrapper: (module ArgWrapper) ResettableLazy.t;
  invariant_parser: InvariantParser.t ResettableLazy.t;
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

module ParamParser (R : Request) = struct
  let parse params =
    let maybe_params =
      params
      |> Option.map_default Structured.yojson_of_t `Null
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

module Function = struct
  type t = {
    funName: string;
    location: CilType.Location.t;
  } [@@deriving eq, ord, hash, yojson]

  let filterFunctions = function
    | Cil.GFun (fd, loc) -> Some {funName = fd.svar.vname; location = loc}
    | _ -> None

  let getFunctionsList files = List.filter_map filterFunctions files
end

let handle_request (serv: t) (request: Request.t): Response.t =
  match Hashtbl.find_option registry request.method_ with
  | Some (module R) ->
    let module Parser = ParamParser (R) in
    begin match Parser.parse request.params with
      | Ok params ->
        begin try
            Maingoblint.reset_stats ();
            let r =
              R.process params serv
              |> R.response_to_yojson
              |> Response.ok request.id
            in
            Maingoblint.do_stats ();
            r
          with Response.Error.E error ->
            Response.error request.id error
        end
      | Error message ->
        Response.(Error.make ~code:InvalidParams ~message () |> error request.id)
    end
  | _ ->
    Response.(Error.make ~code:MethodNotFound ~message:request.method_ () |> error request.id)

let handle_packet (serv: t) (packet: Packet.t) =
  let response_packet: Packet.t option = match packet with
    | Request request -> Some (Response (handle_request serv request))
    | Batch_call subpackets ->
      let responses = List.filter_map (function
          | `Request request -> Some (handle_request serv request)
          | _ -> None (* ignore others for now *)
        ) subpackets in
      Some (Batch_response responses)
    | _ -> None (* ignore others for now *)
  in
  match response_packet with
  | Some response_packet ->
    Packet.yojson_of_t response_packet |> Yojson.Safe.to_string |> IO.write_line serv.output;
    IO.flush serv.output
  | None -> ()

let serve serv =
  serv.input
  |> Lexing.from_channel
  |> Yojson.Safe.seq_from_lexbuf (Yojson.init_lexer ())
  |> Seq.map Packet.t_of_yojson
  |> Seq.iter (handle_packet serv)

(** Is node valid for lookup by location?
    Used for abstract debugging breakpoints. *)
let is_server_node cfgnode =
  let loc = UpdateCil.getLoc cfgnode in
  not loc.synthetic

let arg_wrapper: (module ArgWrapper) ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      let module Arg = (val (Option.get_exn !ArgTools.current_arg Response.Error.(E (make ~code:RequestFailed ~message:"not analyzed or arg disabled" ())))) in
      let module Locator = WitnessUtil.Locator (Arg.Node) in
      let module StringH = Hashtbl.Make (Printable.Strings) in

      let locator = Locator.create () in
      let ids = StringH.create 113 in
      let cfg_nodes = StringH.create 113 in
      Arg.iter_nodes (fun n ->
          let cfgnode = Arg.Node.cfgnode n in
          let loc = UpdateCil.getLoc cfgnode in
          if is_server_node cfgnode then
            Locator.add locator loc n;
          StringH.replace ids (Arg.Node.to_string n) n;
          StringH.add cfg_nodes (Node.show_id cfgnode) n (* add for find_all *)
        );

      let module ArgWrapper =
      struct
        module Arg = Arg
        module Locator = Locator
        let locator = locator
        let find_node = StringH.find ids
        let find_cfg_node = StringH.find_all cfg_nodes
      end
      in
      (module ArgWrapper: ArgWrapper)
    )

let invariant_parser: InvariantParser.t ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      InvariantParser.create !Cilfacade.current_file
    )

let make ?(input=stdin) ?(output=stdout) file : t =
  let max_ids =
    match file with
    | Some file -> MaxIdUtil.get_file_max_ids file
    | None -> MaxIdUtil.get_file_max_ids Cil.dummyFile (* TODO: avoid this altogether *)
  in
  {
    file;
    max_ids;
    arg_wrapper;
    invariant_parser;
    input;
    output
  }

let bind () =
  match GobConfig.get_string "server.mode" with
  | "stdio" ->
    Logs.Result.use_stdout := false;
    (None, None)
  | "unix" ->
    let path = GobConfig.get_string "server.unix-socket" in
    if Sys.file_exists path then
      Sys.remove path;
    let socket = Unix.socket PF_UNIX SOCK_STREAM 0 in
    Unix.bind socket (ADDR_UNIX path);
    Unix.listen socket 1;
    let conn, _ = Unix.accept socket in
    Unix.close socket;
    Sys.remove path;
    (Some (Unix.input_of_descr conn), Some (Unix.output_of_descr conn))
  | _ -> assert false

let start file =
  let input, output = bind () in
  GobConfig.set_bool "incremental.save" true;
  Maingoblint.do_stats (); (* print pre-server stats just in case *)
  serve (make file ?input ?output)

let reparse (s: t) =
  if GobConfig.get_bool "server.reparse" then (
    GoblintDir.init ();
    let file = Fun.protect ~finally:GoblintDir.finalize Maingoblint.preprocess_parse_merge in
    begin match s.file with
      | None ->
        let max_ids = MaxIdUtil.get_file_max_ids file in
        s.max_ids <- max_ids
      | Some _ ->
        ()
    end;
    (file, true)
  )
  else
    (Option.get s.file, false)

(* Only called when the file has not been reparsed, so we can skip the expensive CFG comparison. *)
let virtual_changes file =
  let eq ?(matchVars=true) ?(matchFuns=true) ?(renameDetection=false) _ _ _ gc_old (gc_new: CompareCIL.global_col) ((change_info : CompareCIL.change_info), final_matches) = (match gc_new.def with
      | Some (Fun fdec) when CompareCIL.should_reanalyze fdec ->
        change_info.exclude_from_rel_destab <- CompareCIL.VarinfoSet.add fdec.svar change_info.exclude_from_rel_destab
      | _ -> change_info.unchanged <- {old = gc_old; current= gc_new} :: change_info.unchanged);
    change_info, final_matches
  in
  CompareCIL.compareCilFiles ~eq file file

let increment_data (s: t) file reparsed = match Serialize.Cache.get_opt_data SolverData with
  | Some solver_data when reparsed ->
    let s_file = Option.get s.file in
    let changes = CompareCIL.compareCilFiles s_file file in
    s.max_ids <- UpdateCil.update_ids s_file s.max_ids file changes;
    (* TODO: get globals for restarting from config *)
    Some { server = true; Analyses.changes; solver_data; restarting = [] }, false
  | Some solver_data ->
    let changes = virtual_changes file in
    (* TODO: get globals for restarting from config *)
    Some { server = true; Analyses.changes; solver_data; restarting = [] }, false
  | _ -> None, true


module Locator = WitnessUtil.Locator (Node)
let node_locator: Locator.t ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      let module Cfg = (val !MyCFG.current_cfg) in
      let locator = Locator.create () in

      (* DFS, copied from CfgTools.find_backwards_reachable *)
      let module NH = MyCFG.NodeH in
      let reachable = NH.create 100 in
      let rec iter_node node =
        if not (NH.mem reachable node) then begin
          NH.replace reachable node ();
          let loc = UpdateCil.getLoc node in
          if is_server_node node then
            Locator.add locator loc node;
          List.iter (fun (_, prev_node) ->
              iter_node prev_node
            ) (Cfg.prev node)
        end
      in

      Cil.iterGlobals !Cilfacade.current_file (function
          | GFun (fd, _) ->
            let return_node = Node.Function fd in
            iter_node return_node
          | _ -> ()
        );

      locator
    )

let analyze ?(reset=false) (s: t) =
  Messages.Table.(MH.clear messages_table);
  Messages.(Table.MH.clear final_table);
  Messages.Table.messages_list := [];
  let file, reparsed = reparse s in
  if reset then (
    let max_ids = MaxIdUtil.get_file_max_ids file in
    s.max_ids <- max_ids;
    Serialize.Cache.reset_data SolverData;
    Serialize.Cache.reset_data AnalysisData);
  let increment_data, fresh = increment_data s file reparsed in
  ResettableLazy.reset node_locator;
  ResettableLazy.reset s.arg_wrapper;
  ResettableLazy.reset s.invariant_parser;
  Cilfacade.reset_lazy ();
  InvariantCil.reset_lazy ();
  WideningThresholds.reset_lazy ();
  IntDomain.reset_lazy ();
  FloatDomain.reset_lazy ();
  StringDomain.reset_lazy ();
  PrecisionUtil.reset_lazy ();
  ApronDomain.reset_lazy ();
  AutoTune.reset_lazy ();
  LibraryFunctions.reset_lazy ();
  Access.reset ();
  s.file <- Some file;
  GobConfig.set_bool "incremental.load" (not fresh);
  Fun.protect ~finally:(fun () ->
      GobConfig.set_bool "incremental.load" true
    ) (fun () ->
      Maingoblint.do_analyze increment_data (Option.get s.file);
      Maingoblint.do_gobview (Option.get s.file);
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
        (* TODO: generalize VerifyError for AnalysisState.unsound_both_branches_dead *)
        {status = if !AnalysisState.verified = Some false then VerifyError else Success}
      with
      | Sys.Break ->
        {status = Aborted}
      | Maingoblint.FrontendError message ->
        Response.Error.(raise (make ~code:RequestFailed ~message ()))
  end);

  register (module struct
    let name = "config"
    type params = string * Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    (* TODO: Make it possible to change the non-optional parameters. (i.e., the set of input files) *)
    (* TODO: Check options for compatibility with the incremental analysis. *)
    let process (conf, json) _ =
      try
        GobConfig.set_auto conf (Yojson.Safe.to_string json);
        Maingoblint.handle_options ();
      with exn when GobExn.catch_all_filter exn -> (* TODO: Be more specific in what we catch. *)
        Response.Error.(raise (of_exn exn))
  end);

  register (module struct
    let name = "reset_config"
    type params = unit [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process () _ =
      try
        GobConfig.set_conf Options.defaults;
        Maingoblint.parse_arguments ();
      with exn when GobExn.catch_all_filter exn -> (* TODO: Be more specific in what we catch. *)
        Response.Error.(raise (of_exn exn))
  end);

  register (module struct
    let name = "merge_config"
    type params = Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process json _ =
      try
        GobConfig.merge json;
        Maingoblint.handle_options ();
      with exn when GobExn.catch_all_filter exn -> (* TODO: Be more specific in what we catch. *)
        Response.Error.(raise (of_exn exn))
  end);

  register (module struct
    let name = "read_config"
    type params = { fname: string } [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process { fname } _ =
      try
        GobConfig.merge_file (Fpath.v fname);
        Maingoblint.handle_options ();
      with exn when GobExn.catch_all_filter exn -> (* TODO: Be more specific in what we catch. *)
        Response.Error.(raise (of_exn exn))
  end);

  register (module struct
    let name = "messages"
    type params = unit [@@deriving of_yojson]
    type response = Messages.Message.t list [@@deriving to_yojson]
    let process () _ = Messages.Table.to_list ()
  end);

  register (module struct
    let name = "files"
    type params = unit [@@deriving of_yojson]
    type response = Yojson.Safe.t [@@deriving to_yojson]
    let process () _ = Preprocessor.dependencies_to_yojson ()
  end);

  register (module struct
    let name = "pre_files"
    type params = unit [@@deriving of_yojson]
    type response = Yojson.Safe.t [@@deriving to_yojson]
    let process () s =
      try
        if GobConfig.get_bool "server.reparse" then (
          GoblintDir.init ();
          Fun.protect ~finally:GoblintDir.finalize (fun () ->
              ignore Maingoblint.(preprocess_files () |> parse_preprocessed)
            )
        );
        Preprocessor.dependencies_to_yojson ()
      with Maingoblint.FrontendError message ->
        Response.Error.(raise (make ~code:RequestFailed ~message ()))
  end);

  register (module struct
    let name = "functions"
    type params = unit [@@deriving of_yojson]
    type response = Function.t list [@@deriving to_yojson]
    let process () serv =
      match serv.file with
      | Some file -> Function.getFunctionsList file.globals
      | None -> Response.Error.(raise (make ~code:RequestFailed ~message:"not analyzed" ()))
  end);

  register (module struct
    let name = "cil/varinfos"
    type params = unit [@@deriving of_yojson]
    type varinfo_data = {
      vid: int;
      name: string;
      type_: CilType.Typ.t [@key "type"];
      location: CilType.Location.t;
      original_name: string option;
      role: string;
      function_: CilType.Fundec.t option [@key "function"] [@default None];
    } [@@deriving to_yojson]
    type response = varinfo_data list [@@deriving to_yojson]
    let process () serv =
      Cilfacade.VarinfoH.fold (fun vi role acc ->
          let role_str = match role with
            | Cilfacade.Formal _ -> "formal"
            | Local _ -> "local"
            | Function -> "function"
            | Global -> "global"
          in
          let function_ = match role with
            | Cilfacade.Formal fd
            | Local fd ->
              Some fd
            | Function
            | Global ->
              None
          in
          let data = {
            vid = vi.vid;
            name = vi.vname;
            type_ = vi.vtype;
            location = vi.vdecl;
            original_name = Cilfacade.find_original_name vi;
            role = role_str;
            function_;
          }
          in
          data :: acc
        ) (ResettableLazy.force Cilfacade.varinfo_roles) []
  end);

  register (module struct
    let name = "richvarinfos"
    type params = unit [@@deriving of_yojson]
    type varinfo_data = {
      vid: int;
      name: string;
      description: string;
    } [@@deriving to_yojson]
    type response = varinfo_data list [@@deriving to_yojson]
    let process () serv =
      !RichVarinfo.BiVarinfoMap.Collection.mappings
      |> List.concat_map (fun (module VarinfoMap: RichVarinfo.BiVarinfoMap.S) ->
          VarinfoMap.bindings ()
          |> List.map (fun (x, (vi: Cil.varinfo)) ->
              {
                vid = vi.vid;
                name = vi.vname;
                description = VarinfoMap.describe_varinfo vi x;
              }
            )
        )
  end);

  register (module struct
    let name = "cfg"
    type params = { fname: string }  [@@deriving of_yojson]
    type response = { cfg : string } [@@deriving to_yojson]
    let process { fname } serv =
      let fundec = Cilfacade.find_name_fundec fname in
      let live _ = true in (* TODO: fix this *)
      let cfg = CfgTools.sprint_fundec_html_dot (module (val !MyCFG.current_cfg: MyCFG.CfgBidirSkip): MyCFG.CfgBidir) live fundec in
      { cfg }
  end);

  register (module struct
    let name = "cfg/lookup"
    type params = {
      node: string option [@default None];
      location: CilType.Location.t option [@default None];
    } [@@deriving of_yojson]
    type response = {
      node: string;
      location: CilType.Location.t;
      function_: CilType.Fundec.t [@key "function"];
      next: (Edge.t list * string) list;
      prev: (Edge.t list * string) list;
    } [@@deriving to_yojson]
    let process (params: params) serv =
      let node = match params.node, params.location with
        | Some node_id, None ->
          begin try
              Node.of_id node_id
            with Not_found ->
              Response.Error.(raise (make ~code:RequestFailed ~message:"not analyzed or non-existent node" ()))
          end
        | None, Some location ->
          let node_opt =
            let open GobOption.Syntax in
            let* nodes = Locator.find_opt (ResettableLazy.force node_locator) location in
            Locator.ES.choose_opt nodes
          in
          Option.get_exn node_opt Response.Error.(E (make ~code:RequestFailed ~message:"cannot find node for location" ()))
        | _, _ ->
          Response.Error.(raise (make ~code:RequestFailed ~message:"requires node xor location" ()))
      in
      let node_id = Node.show_id node in
      let location = UpdateCil.getLoc node in
      let function_ = Node.find_fundec node in
      let module Cfg = (val !MyCFG.current_cfg) in
      let next =
        Cfg.next node
        |> List.map (fun (edges, to_node) ->
            (List.map snd edges, Node.show_id to_node)
          )
      in
      let prev =
        Cfg.prev node
        |> List.map (fun (edges, to_node) ->
            (List.map snd edges, Node.show_id to_node)
          )
      in
      {node = node_id; location; function_; next; prev}
  end);

  register (module struct
    let name = "arg/dot"
    type params = unit [@@deriving of_yojson]
    type response = {
      arg: string
    } [@@deriving to_yojson]
    let process () serv =
      let module ArgWrapper = (val (ResettableLazy.force serv.arg_wrapper)) in
      let module NoExtraNodeStyle =
      struct
        type node = ArgWrapper.Arg.Node.t
        let extra_node_styles node = []
      end
      in
      let module ArgDot = ArgTools.Dot (ArgWrapper.Arg) (NoExtraNodeStyle) in
      let arg = Format.asprintf "%t" ArgDot.dot in
      {arg}
  end);

  register (module struct
    let name = "arg/lookup"
    type params = {
      node: string option [@default None];
      location: CilType.Location.t option [@default None];
      cfg_node: string option [@default None];
    } [@@deriving of_yojson]

    type edge_node = {
      edge: MyARG.inline_edge;
      node: string;
      cfg_node: string;
      context: string;
      path: string;
      location: CilType.Location.t;
      function_: CilType.Fundec.t [@key "function"];
    } [@@deriving to_yojson]
    type one_response = {
      node: string;
      cfg_node: string;
      context: string;
      path: string;
      location: CilType.Location.t;
      function_: CilType.Fundec.t [@key "function"];
      next: edge_node list;
      prev: edge_node list;
    } [@@deriving to_yojson]
    type response = one_response list [@@deriving to_yojson]

    let process (params: params) serv =
      let module ArgWrapper = (val (ResettableLazy.force serv.arg_wrapper)) in
      let open ArgWrapper in
      let open GobList.Syntax in
      let+ n: Arg.Node.t = match params.node, params.location, params.cfg_node with
        | None, None, None ->
          [Arg.main_entry]
        | Some node_id, None, None ->
          begin try
              [ArgWrapper.find_node node_id]
            with Not_found ->
              [] (* non-existent node *)
          end
        | None, Some location, None ->
          let nodes_opt =
            let open GobOption.Syntax in
            let+ nodes = Locator.find_opt locator location in
            Locator.ES.elements nodes
          in
          Option.default [] nodes_opt (* cannot find node for location *)
        | None, None, Some cfg_node ->
          ArgWrapper.find_cfg_node cfg_node
        | _, _, _ ->
          Response.Error.(raise (make ~code:RequestFailed ~message:"requires at most one of node, location and cfg_node" ()))
      in
      let cfg_node = Arg.Node.cfgnode n in
      let cfg_node_id = Node.show_id cfg_node in
      let location = UpdateCil.getLoc cfg_node in
      let next =
        Arg.next n
        |> List.map (fun (edge, to_node) ->
            let cfg_to_node = Arg.Node.cfgnode to_node in
            {
              edge;
              node = Arg.Node.to_string to_node;
              cfg_node = Node.show_id cfg_to_node;
              context = string_of_int (Arg.Node.context_id to_node);
              path = string_of_int (Arg.Node.path_id to_node);
              location = UpdateCil.getLoc cfg_to_node;
              function_ = Node.find_fundec cfg_to_node;
            }
          )
      in
      let prev =
        Arg.prev n
        |> List.map (fun (edge, to_node) ->
            let cfg_to_node = Arg.Node.cfgnode to_node in
            {
              edge;
              node = Arg.Node.to_string to_node;
              cfg_node = Node.show_id cfg_to_node;
              context = string_of_int (Arg.Node.context_id to_node);
              path = string_of_int (Arg.Node.path_id to_node);
              location = UpdateCil.getLoc cfg_to_node;
              function_ = Node.find_fundec cfg_to_node;
            }
          )
      in
      {
        node = Arg.Node.to_string n;
        cfg_node = cfg_node_id;
        context = string_of_int (Arg.Node.context_id n);
        path = string_of_int (Arg.Node.path_id n);
        location;
        function_ = Node.find_fundec cfg_node;
        next;
        prev
      }
  end);

  register (module struct
    let name = "node_state"
    type params = { nid: string }  [@@deriving of_yojson]
    type response = Yojson.Safe.t [@@deriving to_yojson]
    let process { nid } serv =
      match Node.of_id nid with
      | n ->
        begin match !Control.current_node_state_json n with
          | Some json -> json
          | None -> Response.Error.(raise (make ~code:RequestFailed ~message:"not analyzed, non-existent or dead node" ()))
        end
      | exception Not_found -> Response.Error.(raise (make ~code:RequestFailed ~message:"not analyzed or non-existent node" ()))
  end);

  register (module struct
    let name = "global-state"
    type params = {
      vid: int option [@default None];
      node: string option [@default None];
    } [@@deriving of_yojson]
    type response = Yojson.Safe.t [@@deriving to_yojson]
    let process (params: params) serv =
      let vq_opt = match params.vid, params.node with
        | None, None ->
          None
        | Some vid, None ->
          let vi = {Cil.dummyFunDec.svar with vid} in (* Equal to actual varinfo by vid. *)
          Some (VarQuery.Global vi)
        | None, Some node_id ->
          let node = try
              Node.of_id node_id
            with Not_found ->
              Response.Error.(raise (make ~code:RequestFailed ~message:"not analyzed or non-existent node" ()))
          in
          Some (VarQuery.Node {node; fundec = None})
        | Some _, Some _ ->
          Response.Error.(raise (make ~code:RequestFailed ~message:"requires at most one of vid and node" ()))
      in
      !Control.current_varquery_global_state_json vq_opt
  end);

  register (module struct
    let name = "arg/state"
    type params = {
      node: string
    } [@@deriving of_yojson]
    type response = Yojson.Safe.t [@@deriving to_yojson]
    let process {node} serv =
      let module ArgWrapper = (val (ResettableLazy.force serv.arg_wrapper)) in
      match ArgWrapper.find_node node with
      | n ->
        begin match ArgWrapper.Arg.query n DYojson with
          | `Lifted json -> json
          | (`Bot | `Top) as r -> Response.Error.(raise (make ~code:RequestFailed ~message:("query returned " ^ Queries.FlatYojson.show r) ()))
        end
      | exception Not_found -> Response.Error.(raise (make ~code:RequestFailed ~message:"non-existent node" ()))
  end);

  register (module struct
    let name = "arg/eval"
    type params = {
      node: string;
      exp: string option [@default None];
      vid: int option [@default None]; (* eval varinfo by vid to avoid exp parsing problems *)
    } [@@deriving of_yojson]
    type response = Queries.VD.t [@@deriving to_yojson]
    let process (params: params) serv =
      let module ArgWrapper = (val (ResettableLazy.force serv.arg_wrapper)) in
      let open ArgWrapper in
      match ArgWrapper.find_node params.node with
      | n ->
        let exp = match params.exp, params.vid with
          | Some exp, None ->
            begin match InvariantParser.parse_cabs exp with
              | Ok exp_cabs ->
                let cfg_node = Arg.Node.cfgnode n in
                let fundec = Node.find_fundec cfg_node in
                let loc = UpdateCil.getLoc cfg_node in

                begin match InvariantParser.parse_cil (ResettableLazy.force serv.invariant_parser) ~fundec ~loc exp_cabs with
                  | Ok exp -> exp
                  | Error e ->
                    Response.Error.(raise (make ~code:RequestFailed ~message:"CIL couldn't parse expression (undefined variables or side effects)" ()))
                end
              | Error e ->
                Response.Error.(raise (make ~code:RequestFailed ~message:"Frontc couldn't parse expression (invalid syntax)" ()))
            end
          | None, Some vid ->
            let vi = {Cil.dummyFunDec.svar with vid} in (* Equal to actual varinfo by vid. *)
            Lval (Cil.var vi)
          | _, _ ->
            Response.Error.(raise (make ~code:RequestFailed ~message:"requires exp xor vid" ()))
        in
        Arg.query n (EvalValue exp)
      | exception Not_found -> Response.Error.(raise (make ~code:RequestFailed ~message:"non-existent node" ()))
  end);

  register (module struct
    let name = "arg/eval-int"
    type params = {
      node: string;
      exp: string;
    } [@@deriving of_yojson]
    type response = {
      raw: Yojson.Safe.t;
      int: GobZ.t option;
      bool: bool option;
    } [@@deriving to_yojson]
    let process {node; exp} serv =
      let module ArgWrapper = (val (ResettableLazy.force serv.arg_wrapper)) in
      let open ArgWrapper in
      match ArgWrapper.find_node node with
      | n ->
        begin match InvariantParser.parse_cabs exp with
          | Ok exp_cabs ->
            let cfg_node = Arg.Node.cfgnode n in
            let fundec = Node.find_fundec cfg_node in
            let loc = UpdateCil.getLoc cfg_node in

            begin match InvariantParser.parse_cil (ResettableLazy.force serv.invariant_parser) ~fundec ~loc exp_cabs with
              | Ok exp ->
                let x = Arg.query n (EvalInt exp) in
                {
                  raw = Queries.ID.to_yojson x;
                  int = Queries.ID.to_int x;
                  bool = Queries.ID.to_bool x; (* Separate, because Not{0} has to_int = None, but to_bool = Some true. *)
                }
              | Error e ->
                Response.Error.(raise (make ~code:RequestFailed ~message:"CIL couldn't parse expression (undefined variables or side effects)" ()))
            end
          | Error e ->
            Response.Error.(raise (make ~code:RequestFailed ~message:"Frontc couldn't parse expression (invalid syntax)" ()))
        end
      | exception Not_found -> Response.Error.(raise (make ~code:RequestFailed ~message:"non-existent node" ()))
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
