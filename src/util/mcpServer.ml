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

(** MCP Server state *)
type t = {
  server: Server.t;
  mutable analyzed: bool;
}

let make () : t =
  (* Create server with stdin/stdout streams *)
  let server = Server.make None in
  {
    server;
    analyzed = false;
  }

(** JSON Schema helpers *)
let string_schema = `Assoc [("type", `String "string")]
let bool_schema = `Assoc [("type", `String "boolean")]

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
    name = "get_functions";
    description = Some "Get list of functions found in the analyzed code.";
    inputSchema = object_schema [] []
  };
  {
    name = "query_state";
    description = Some "Query the analysis state at a specific program location (node).";
    inputSchema = object_schema [
      ("node", string_schema)
    ] ["node"]
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
        let value = Yojson.Safe.Util.member "value" args in
        GobConfig.set_auto option (Yojson.Safe.to_string value);
        Maingoblint.handle_options ();
        ToolResult.make_text (Printf.sprintf "Configuration option '%s' set successfully" option)
      
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
        
        (* Run analysis *)
        Server.analyze mcp_serv.server ~reset;
        mcp_serv.analyzed <- true;
        
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
        
        (* Run analysis *)
        Server.analyze mcp_serv.server ~reset;
        mcp_serv.analyzed <- true;
        
        let status = if !AnalysisState.verified = Some false then "VerifyError" else "Success" in
        let msg_count = List.length (Messages.Table.to_list ()) in
        ToolResult.make_text (Printf.sprintf "Analysis completed with status: %s. Found %d message(s)." status msg_count)
      
      | "get_messages" ->
        let messages = Messages.Table.to_list () in
        let json = `List (List.map Messages.Message.to_yojson messages) in
        ToolResult.make_text (Yojson.Safe.to_string json)
      
      | "get_functions" ->
        begin match mcp_serv.server.file with
          | Some file ->
            let functions = Server.Function.getFunctionsList file.globals in
            let json = `List (List.map Server.Function.to_yojson functions) in
            ToolResult.make_text (Yojson.Safe.to_string json)
          | None ->
            ToolResult.make_error "No file has been analyzed yet"
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
        Logs.error "JSON parse error: %s" msg
      | exn ->
        Logs.error "Error handling packet: %s" (Printexc.to_string exn)
      end;
      read_loop ()
    with
    | End_of_file -> ()
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
