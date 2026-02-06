(** MCP (Model Context Protocol) server for Goblint.
    
    This module provides an MCP-compliant server that allows LLMs and other
    clients to interact with Goblint's static analysis capabilities.
    
    The MCP protocol is based on JSON-RPC 2.0 and provides standardized
    methods for tools, resources, and prompts.
*)

open Batteries
open Jsonrpc
open GoblintCil

module Info = struct
  type t = {
    name: string;
    version: string;
  } [@@deriving yojson]
end

module ToolInput = struct
  type t = {
    name: string;
    description: string option [@default None];
    inputSchema: Yojson.Safe.t; (* JSON Schema *)
  } [@@deriving yojson]
end

module ToolCall = struct
  type t = {
    name: string;
    arguments: Yojson.Safe.t option [@default None];
  } [@@deriving yojson]
end

module ToolResult = struct
  type content_text = {
    type_: string [@key "type"];
    text: string;
  } [@@deriving to_yojson]
  
  type t = {
    content: content_text list;
    isError: bool option [@default None];
  } [@@deriving to_yojson]
  
  let make_text text = {
    content = [{type_ = "text"; text}];
    isError = None;
  }
  
  let make_error text = {
    content = [{type_ = "text"; text}];
    isError = Some true;
  }
end

(** MCP Server state 
    
    ResettableLazy fields (arg_wrapper, invariant_parser, node_locator) need to be
    reset between analyses to reflect the new analysis results and parsed files. *)
type t = {
  mutable file: Cil.file option;
  mutable max_ids: MaxIdUtil.max_ids;
  mutable analyzed: bool;
  arg_wrapper: (module ServerUtil.ArgWrapper) ResettableLazy.t;
  invariant_parser: WitnessUtil.InvariantParser.t ResettableLazy.t;
  node_locator: WitnessUtil.Locator(Node).t ResettableLazy.t;
}

let make () : t =
  {
    file = None;
    max_ids = MaxIdUtil.get_file_max_ids Cil.dummyFile;
    analyzed = false;
    arg_wrapper = ServerUtil.create_arg_wrapper ();
    invariant_parser = ServerUtil.create_invariant_parser ();
    node_locator = ServerUtil.create_node_locator ();
  }

(** Analyze the configured files directly *)
let analyze ?(reset=false) (mcp_serv: t) =
  Messages.Table.(MH.clear messages_table);
  Messages.(Table.MH.clear final_table);
  Messages.Table.messages_list := [];
  
  (* Preprocess and parse files *)
  GoblintDir.init ();
  let file = Fun.protect ~finally:GoblintDir.finalize Maingoblint.preprocess_parse_merge in
  
  if reset then (
    let max_ids = MaxIdUtil.get_file_max_ids file in
    mcp_serv.max_ids <- max_ids;
    Serialize.Cache.reset_data SolverData;
    Serialize.Cache.reset_data AnalysisData;
  );
  
  (* Reset lazy modules *)
  ResettableLazy.reset mcp_serv.node_locator;
  ResettableLazy.reset mcp_serv.arg_wrapper;
  ResettableLazy.reset mcp_serv.invariant_parser;
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
  
  mcp_serv.file <- Some file;
  
  (* Run analysis *)
  Maingoblint.do_analyze None file;
  Maingoblint.do_gobview file;
  
  (* Mark as analyzed *)
  mcp_serv.analyzed <- true

(** JSON Schema helpers *)
let string_schema = `Assoc [("type", `String "string")]
let bool_schema = `Assoc [("type", `String "boolean")]
let int_schema = `Assoc [("type", `String "integer")]

let object_schema properties required = `Assoc [
  ("type", `String "object");
  ("properties", `Assoc properties);
  ("required", `List (List.map (fun s -> `String s) required))
]

(** Define MCP tools that wrap Goblint functionality *)
let tools : ToolInput.t list = [
  {
    name = "configure";
    description = Some "Configure Goblint analysis options. Use this to set configuration parameters before running analysis.";
    inputSchema = object_schema [
      ("option", string_schema);
      ("value", `Assoc [("type", `String "string")])
    ] ["option"; "value"]
  };
  {
    name = "merge_config";
    description = Some "Merge JSON configuration into current Goblint configuration.";
    inputSchema = object_schema [
      ("config", `Assoc [("type", `String "object")])
    ] ["config"]
  };
  {
    name = "read_config";
    description = Some "Read and merge configuration from a file.";
    inputSchema = object_schema [
      ("file", string_schema)
    ] ["file"]
  };
  {
    name = "reset_config";
    description = Some "Reset Goblint configuration to default values.";
    inputSchema = object_schema [] []
  };
  {
    name = "analyze_file";
    description = Some "Run Goblint analysis on a single C source file. Returns analysis status and any warnings or errors found.";
    inputSchema = object_schema [
      ("file", string_schema);
      ("reset", bool_schema)
    ] ["file"]
  };
  {
    name = "analyze_project";
    description = Some "Run Goblint analysis on a project using a compilation database (compile_commands.json). This allows analyzing complex projects with multiple source files.";
    inputSchema = object_schema [
      ("compilation_database", string_schema);
      ("reset", bool_schema)
    ] ["compilation_database"]
  };
  {
    name = "get_messages";
    description = Some "Get analysis messages (warnings, errors, etc.) from the last analysis run.";
    inputSchema = object_schema [] []
  };
  {
    name = "get_files";
    description = Some "Get list of files that were analyzed, including dependencies.";
    inputSchema = object_schema [] []
  };
  {
    name = "get_functions";
    description = Some "Get list of functions found in the analyzed code.";
    inputSchema = object_schema [] []
  };
  {
    name = "get_varinfos";
    description = Some "Get information about all variables in the analyzed code (vid, name, type, location, role).";
    inputSchema = object_schema [] []
  };
  {
    name = "get_cfg";
    description = Some "Get control flow graph (CFG) for a specific function in DOT format.";
    inputSchema = object_schema [
      ("function", string_schema)
    ] ["function"]
  };
  {
    name = "cfg_lookup";
    description = Some "Look up CFG node information by node ID or location.";
    inputSchema = object_schema [
      ("node", string_schema);
      ("location", `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("file", string_schema);
          ("line", int_schema);
          ("column", int_schema)
        ])
      ])
    ] []
  };
  {
    name = "arg_lookup";
    description = Some "Look up ARG (Abstract Reachability Graph) node information.";
    inputSchema = object_schema [
      ("node", string_schema);
      ("cfg_node", string_schema);
      ("location", `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("file", string_schema);
          ("line", int_schema);
          ("column", int_schema)
        ])
      ])
    ] []
  };
  {
    name = "arg_eval";
    description = Some "Evaluate an expression at a specific ARG node.";
    inputSchema = object_schema [
      ("node", string_schema);
      ("expression", string_schema);
      ("vid", int_schema)
    ] ["node"]
  };
  {
    name = "arg_eval_int";
    description = Some "Evaluate an integer expression at a specific ARG node.";
    inputSchema = object_schema [
      ("node", string_schema);
      ("expression", string_schema)
    ] ["node"; "expression"]
  };
  {
    name = "query_state";
    description = Some "Query the analysis state at a specific program location (node).";
    inputSchema = object_schema [
      ("node", string_schema)
    ] ["node"]
  };
  {
    name = "global_state";
    description = Some "Query global state for a variable or at a specific node.";
    inputSchema = object_schema [
      ("vid", int_schema);
      ("node", string_schema)
    ] []
  };
]

(** Handle MCP initialize request *)
let handle_initialize () =
  `Assoc [
    ("protocolVersion", `String "2024-11-05");
    ("capabilities", `Assoc [
      ("tools", `Assoc []);
    ]);
    ("serverInfo", Info.to_yojson {
      name = "goblint-mcp-server";
      version = Goblint_build_info.version;
    })
  ]

(** Handle MCP tools/list request *)
let handle_tools_list () =
  `Assoc [
    ("tools", `List (List.map ToolInput.to_yojson tools))
  ]

(** Handle MCP tools/call request *)
let handle_tools_call (mcp_serv: t) (call: ToolCall.t) =
  try
    let args = Option.default `Null call.arguments in
    let result = match call.name with
      | "configure" ->
        let option = Yojson.Safe.Util.member "option" args |> Yojson.Safe.Util.to_string in
        let value_json = Yojson.Safe.Util.member "value" args in
        (* Convert JSON value to appropriate string representation for GobConfig *)
        let value_str = match value_json with
          | `String s -> Printf.sprintf "\"%s\"" s  (* Keep quotes for string values *)
          | `Bool b -> if b then "true" else "false"
          | `Int i -> string_of_int i
          | `Float f -> string_of_float f
          | `Null -> "null"
          | other -> Yojson.Safe.to_string other  (* For arrays, objects, etc. *)
        in
        GobConfig.set_auto option value_str;
        Maingoblint.handle_options ();
        ToolResult.make_text (Printf.sprintf "Configuration option '%s' set successfully" option)
      
      | "merge_config" ->
        let config = Yojson.Safe.Util.member "config" args in
        GobConfig.merge config;
        Maingoblint.handle_options ();
        ToolResult.make_text "Configuration merged successfully"
      
      | "read_config" ->
        let file = Yojson.Safe.Util.member "file" args |> Yojson.Safe.Util.to_string in
        GobConfig.merge_file (Fpath.v file);
        Maingoblint.handle_options ();
        ToolResult.make_text (Printf.sprintf "Configuration loaded from '%s'" file)
      
      | "reset_config" ->
        GobConfig.set_conf Options.defaults;
        Maingoblint.parse_arguments ();
        ToolResult.make_text "Configuration reset to defaults"
      
      | "analyze_file" ->
        let file = Yojson.Safe.Util.member "file" args |> Yojson.Safe.Util.to_string in
        let reset = 
          try Yojson.Safe.Util.member "reset" args |> Yojson.Safe.Util.to_bool
          with _ -> false
        in
        
        (* Set the file to analyze *)
        GobConfig.set_list "files" [`String file];
        
        (* Run analysis directly *)
        analyze mcp_serv ~reset;
        
        let status = if !AnalysisState.verified = Some false then "VerifyError" else "Success" in
        let msg_count = List.length (Messages.Table.to_list ()) in
        ToolResult.make_text (Printf.sprintf "Analysis completed with status: %s. Found %d message(s)." status msg_count)
      
      | "analyze_project" ->
        let comp_db = Yojson.Safe.Util.member "compilation_database" args |> Yojson.Safe.Util.to_string in
        let reset = 
          try Yojson.Safe.Util.member "reset" args |> Yojson.Safe.Util.to_bool
          with _ -> false
        in
        
        (* Set compilation database *)
        GobConfig.set_string "pre.compdb.file" comp_db;
        
        (* Run analysis directly *)
        analyze mcp_serv ~reset;
        
        let status = if !AnalysisState.verified = Some false then "VerifyError" else "Success" in
        let msg_count = List.length (Messages.Table.to_list ()) in
        ToolResult.make_text (Printf.sprintf "Analysis completed with status: %s. Found %d message(s)." status msg_count)
      
      | "get_messages" ->
        let messages = Messages.Table.to_list () in
        let json = `List (List.map Messages.Message.to_yojson messages) in
        ToolResult.make_text (Yojson.Safe.to_string json)
      
      | "get_files" ->
        let files_json = Preprocessor.dependencies_to_yojson () in
        ToolResult.make_text (Yojson.Safe.to_string files_json)
      
      | "get_functions" ->
        begin match mcp_serv.file with
          | Some file ->
            let functions = List.filter_map (function
                | Cil.GFun (fd, loc) -> Some `Assoc [
                    ("funName", `String fd.svar.vname);
                    ("location", CilType.Location.to_yojson loc)
                  ]
                | _ -> None
              ) file.globals
            in
            ToolResult.make_text (Yojson.Safe.to_string (`List functions))
          | None ->
            ToolResult.make_error "No file has been analyzed yet"
        end
      
      | "get_varinfos" ->
        let varinfos = Cilfacade.VarinfoH.fold (fun vi role acc ->
            let role_str = match role with
              | Cilfacade.Formal _ -> "formal"
              | Local _ -> "local"
              | Cilfacade.Function -> "function"
              | Global -> "global"
            in
            let function_ = match role with
              | Cilfacade.Formal fd
              | Local fd -> Some fd
              | Cilfacade.Function
              | Global -> None
            in
            let data = `Assoc [
              ("vid", `Int vi.vid);
              ("name", `String vi.vname);
              ("type", CilType.Typ.to_yojson vi.vtype);
              ("location", CilType.Location.to_yojson vi.vdecl);
              ("original_name", match Cilfacade.find_original_name vi with
                | Some n -> `String n
                | None -> `Null);
              ("role", `String role_str);
              ("function", match function_ with
                | Some fd -> CilType.Fundec.to_yojson fd
                | None -> `Null);
            ]
            in
            data :: acc
          ) (ResettableLazy.force Cilfacade.varinfo_roles) []
        in
        ToolResult.make_text (Yojson.Safe.to_string (`List varinfos))
      
      | "get_cfg" ->
        let fname = Yojson.Safe.Util.member "function" args |> Yojson.Safe.Util.to_string in
        let fundec = Cilfacade.find_name_fundec fname in
        let live _ = true in
        let cfg = CfgTools.sprint_fundec_html_dot (module (val !MyCFG.current_cfg: MyCFG.CfgBidirSkip): MyCFG.CfgBidir) live fundec in
        ToolResult.make_text cfg
      
      | "cfg_lookup" ->
        let node_opt = try Some (Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string) with _ -> None in
        let location_opt = try
            let loc_json = Yojson.Safe.Util.member "location" args in
            Some (CilType.Location.of_yojson loc_json |> Result.get_ok)
          with _ -> None
        in
        let node = match node_opt, location_opt with
          | Some node_id, None ->
            Node.of_id node_id
          | None, Some location ->
            let module Locator = WitnessUtil.Locator (Node) in
            let node_opt =
              let open GobOption.Syntax in
              let* nodes = Locator.find_opt (ResettableLazy.force mcp_serv.node_locator) location in
              Locator.ES.choose_opt nodes
            in
            Option.get_exn node_opt (Failure "cannot find node for location")
          | _, _ ->
            raise (Failure "requires node xor location")
        in
        let node_id = Node.show_id node in
        let location = UpdateCil.getLoc node in
        let function_ = Node.find_fundec node in
        let module Cfg = (val !MyCFG.current_cfg) in
        let next = Cfg.next node |> List.map (fun (edges, to_node) ->
            (List.map snd edges, Node.show_id to_node)
          ) in
        let prev = Cfg.prev node |> List.map (fun (edges, to_node) ->
            (List.map snd edges, Node.show_id to_node)
          ) in
        let result = `Assoc [
          ("node", `String node_id);
          ("location", CilType.Location.to_yojson location);
          ("function", CilType.Fundec.to_yojson function_);
          ("next", `List (List.map (fun (edges, nid) ->
              `Assoc [("edges", `List (List.map Edge.to_yojson edges)); ("node", `String nid)]
            ) next));
          ("prev", `List (List.map (fun (edges, nid) ->
              `Assoc [("edges", `List (List.map Edge.to_yojson edges)); ("node", `String nid)]
            ) prev));
        ] in
        ToolResult.make_text (Yojson.Safe.to_string result)
      
      | "arg_lookup" ->
        let module ArgWrapper = (val (ResettableLazy.force mcp_serv.server.arg_wrapper)) in
        let open ArgWrapper in
        let node_opt = try Some (Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string) with _ -> None in
        let cfg_node_opt = try Some (Yojson.Safe.Util.member "cfg_node" args |> Yojson.Safe.Util.to_string) with _ -> None in
        let location_opt = try
            let loc_json = Yojson.Safe.Util.member "location" args in
            Some (CilType.Location.of_yojson loc_json |> Result.get_ok)
          with _ -> None
        in
        let nodes = match node_opt, location_opt, cfg_node_opt with
          | None, None, None -> [Arg.main_entry]
          | Some node_id, None, None ->
            begin try [ArgWrapper.find_node node_id]
            with Not_found -> []
            end
          | None, Some location, None ->
            let nodes_opt =
              let open GobOption.Syntax in
              let+ nodes = Locator.find_opt locator location in
              Locator.ES.elements nodes
            in
            Option.default [] nodes_opt
          | None, None, Some cfg_node ->
            ArgWrapper.find_cfg_node cfg_node
          | _, _, _ ->
            raise (Failure "requires at most one of node, location and cfg_node")
        in
        let results = List.map (fun n ->
            let cfg_node = Arg.Node.cfgnode n in
            let cfg_node_id = Node.show_id cfg_node in
            let location = UpdateCil.getLoc cfg_node in
            let next = Arg.next n |> List.map (fun (edge, to_node) ->
                let cfg_to_node = Arg.Node.cfgnode to_node in
                `Assoc [
                  ("edge", MyARG.inline_edge_to_yojson edge);
                  ("node", `String (Arg.Node.to_string to_node));
                  ("cfg_node", `String (Node.show_id cfg_to_node));
                  ("location", CilType.Location.to_yojson (UpdateCil.getLoc cfg_to_node));
                  ("function", CilType.Fundec.to_yojson (Node.find_fundec cfg_to_node));
                ]
              ) in
            let prev = Arg.prev n |> List.map (fun (edge, to_node) ->
                let cfg_to_node = Arg.Node.cfgnode to_node in
                `Assoc [
                  ("edge", MyARG.inline_edge_to_yojson edge);
                  ("node", `String (Arg.Node.to_string to_node));
                  ("cfg_node", `String (Node.show_id cfg_to_node));
                  ("location", CilType.Location.to_yojson (UpdateCil.getLoc cfg_to_node));
                  ("function", CilType.Fundec.to_yojson (Node.find_fundec cfg_to_node));
                ]
              ) in
            `Assoc [
              ("node", `String (Arg.Node.to_string n));
              ("cfg_node", `String cfg_node_id);
              ("location", CilType.Location.to_yojson location);
              ("function", CilType.Fundec.to_yojson (Node.find_fundec cfg_node));
              ("next", `List next);
              ("prev", `List prev);
            ]
          ) nodes
        in
        ToolResult.make_text (Yojson.Safe.to_string (`List results))
      
      | "arg_eval" ->
        let module ArgWrapper = (val (ResettableLazy.force mcp_serv.server.arg_wrapper)) in
        let open ArgWrapper in
        let node_id = Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string in
        let exp_opt = try Some (Yojson.Safe.Util.member "expression" args |> Yojson.Safe.Util.to_string) with _ -> None in
        let vid_opt = try Some (Yojson.Safe.Util.member "vid" args |> Yojson.Safe.Util.to_int) with _ -> None in
        let n = ArgWrapper.find_node node_id in
        let exp = match exp_opt, vid_opt with
          | Some exp_str, None ->
            begin match WitnessUtil.InvariantParser.parse_cabs exp_str with
              | Ok exp_cabs ->
                let cfg_node = Arg.Node.cfgnode n in
                let fundec = Node.find_fundec cfg_node in
                let loc = UpdateCil.getLoc cfg_node in
                begin match WitnessUtil.InvariantParser.parse_cil (ResettableLazy.force mcp_serv.invariant_parser) ~fundec ~loc exp_cabs with
                  | Ok exp -> exp
                  | Error _ -> raise (Failure "CIL couldn't parse expression")
                end
              | Error _ -> raise (Failure "Frontc couldn't parse expression")
            end
          | None, Some vid ->
            let vi = {Cil.dummyFunDec.svar with vid} in
            Cil.Lval (Cil.var vi)
          | _, _ ->
            raise (Failure "requires expression xor vid")
        in
        let value = Arg.query n (Queries.EvalValue exp) in
        ToolResult.make_text (Yojson.Safe.to_string (Queries.VD.to_yojson value))
      
      | "arg_eval_int" ->
        let module ArgWrapper = (val (ResettableLazy.force mcp_serv.server.arg_wrapper)) in
        let open ArgWrapper in
        let node_id = Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string in
        let exp_str = Yojson.Safe.Util.member "expression" args |> Yojson.Safe.Util.to_string in
        let n = ArgWrapper.find_node node_id in
        begin match WitnessUtil.InvariantParser.parse_cabs exp_str with
          | Ok exp_cabs ->
            let cfg_node = Arg.Node.cfgnode n in
            let fundec = Node.find_fundec cfg_node in
            let loc = UpdateCil.getLoc cfg_node in
            begin match WitnessUtil.InvariantParser.parse_cil (ResettableLazy.force mcp_serv.invariant_parser) ~fundec ~loc exp_cabs with
              | Ok exp ->
                let x = Arg.query n (Queries.EvalInt exp) in
                let result = `Assoc [
                  ("raw", Queries.ID.to_yojson x);
                  ("int", match Queries.ID.to_int x with Some i -> `String (GobZ.to_string i) | None -> `Null);
                  ("bool", match Queries.ID.to_bool x with Some b -> `Bool b | None -> `Null);
                ] in
                ToolResult.make_text (Yojson.Safe.to_string result)
              | Error _ -> ToolResult.make_error "CIL couldn't parse expression"
            end
          | Error _ -> ToolResult.make_error "Frontc couldn't parse expression"
        end
      
      | "query_state" ->
        let node = Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string in
        begin match Node.of_id node with
          | n ->
            begin match !Control.current_node_state_json n with
              | Some json -> ToolResult.make_text (Yojson.Safe.to_string json)
              | None -> ToolResult.make_error "Node state not available (not analyzed, non-existent or dead node)"
            end
          | exception Not_found ->
            ToolResult.make_error "Node not found"
        end
      
      | "global_state" ->
        let vid_opt = try Some (Yojson.Safe.Util.member "vid" args |> Yojson.Safe.Util.to_int) with _ -> None in
        let node_opt = try Some (Yojson.Safe.Util.member "node" args |> Yojson.Safe.Util.to_string) with _ -> None in
        let vq_opt = match vid_opt, node_opt with
          | None, None -> None
          | Some vid, None ->
            let vi = {Cil.dummyFunDec.svar with vid} in
            Some (Goblint_constraint.VarQuery.Global vi)
          | None, Some node_id ->
            let node = Node.of_id node_id in
            Some (Goblint_constraint.VarQuery.Node {node; fundec = None})
          | Some _, Some _ ->
            raise (Failure "requires at most one of vid and node")
        in
        let state_json = !Control.current_varquery_global_state_json vq_opt in
        ToolResult.make_text (Yojson.Safe.to_string state_json)
      
      | _ ->
        ToolResult.make_error (Printf.sprintf "Unknown tool: %s" call.name)
    in
    ToolResult.to_yojson result
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    ToolResult.(to_yojson (make_error ("Invalid argument type: " ^ msg)))
  | Yojson.Safe.Util.Undefined (msg, _) ->
    ToolResult.(to_yojson (make_error ("Missing required argument: " ^ msg)))
  | Maingoblint.FrontendError message ->
    ToolResult.(to_yojson (make_error ("Frontend error: " ^ message)))
  | Failure msg ->
    ToolResult.(to_yojson (make_error msg))
  | Not_found ->
    ToolResult.(to_yojson (make_error "Resource not found"))
  | exn ->
    ToolResult.(to_yojson (make_error ("Error: " ^ Printexc.to_string exn)))

(** Handle an MCP request *)
let handle_mcp_request (mcp_serv: t) (request: Request.t) : Response.t =
  try
    Maingoblint.reset_stats ();
    let result = match request.method_ with
      | "initialize" ->
        handle_initialize ()
      | "initialized" ->
        `Assoc [] (* Empty response for notification *)
      | "tools/list" ->
        handle_tools_list ()
      | "tools/call" ->
        let call = match request.params with
          | Some params ->
            let params_json = Structured.yojson_of_t params in
            begin match ToolCall.of_yojson params_json with
              | Ok call -> call
              | Error err -> raise (Failure ("Invalid tool call parameters: " ^ err))
            end
          | None ->
            raise (Failure "tools/call requires parameters")
        in
        handle_tools_call mcp_serv call
      | "ping" ->
        `Assoc [] (* Simple ping response *)
      | _ ->
        raise (Failure ("Unknown method: " ^ request.method_))
    in
    Maingoblint.do_stats ();
    Response.ok request.id result
  with
  | Failure msg ->
    Response.(Error.make ~code:RequestFailed ~message:msg () |> error request.id)
  | exn ->
    Response.(Error.make ~code:InternalError ~message:(Printexc.to_string exn) () |> error request.id)

(** Handle an MCP packet *)
let handle_packet (mcp_serv: t) (packet: Packet.t) =
  let response_packet: Packet.t option = match packet with
    | Request request ->
      Some (Response (handle_mcp_request mcp_serv request))
    | Batch_call subpackets ->
      let responses = List.filter_map (function
          | `Request request -> Some (handle_mcp_request mcp_serv request)
          | _ -> None
        ) subpackets in
      Some (Batch_response responses)
    | _ -> None
  in
  match response_packet with
  | Some response_packet ->
    Packet.yojson_of_t response_packet |> Yojson.Safe.to_string |> print_endline;
    flush stdout
  | None -> ()

(** Serve MCP requests on stdio *)
let serve mcp_serv =
  (* Read from stdin line by line *)
  let rec read_loop () =
    try
      let line = read_line () in
      begin try
        let json = Yojson.Safe.from_string line in
        let packet = Packet.t_of_yojson json in
        handle_packet mcp_serv packet
      with
      | Yojson.Json_error msg ->
        (* Log JSON parse errors with input context for debugging *)
        let truncated_line = if String.length line > 100 then String.sub line 0 100 ^ "..." else line in
        Logs.error "JSON parse error: %s (input: %s)" msg truncated_line
      | exn ->
        Logs.error "Error handling packet: %s" (Printexc.to_string exn)
      end;
      read_loop ()
    with
    | End_of_file ->
      (* Clean shutdown when stdin is closed *)
      Logs.info "MCP server shutting down (stdin closed)"
  in
  read_loop ()

(** Start the MCP server *)
let start () =
  (* Disable other output that might interfere with JSON-RPC *)
  Logs.Result.use_stdout := false;
  
  (* Initialize Goblint *)
  GobConfig.set_bool "incremental.save" true;
  Maingoblint.do_stats ();
  
  (* Create and run server *)
  let mcp_serv = make () in
  serve mcp_serv
