(** Goblint MCP Server executable
    
    This provides a Model Context Protocol (MCP) server interface to Goblint,
    allowing LLMs and other MCP clients to interact with Goblint's static
    analysis capabilities.
    
    Usage:
      goblint-mcp-server
    
    The server communicates via JSON-RPC 2.0 over stdin/stdout.
*)

open Goblint_lib

let () =
  try
    (* Parse any command-line arguments for initial configuration *)
    Maingoblint.parse_arguments ();
    
    (* Initialize CIL *)
    Cilfacade.init ();
    
    (* Start the MCP server *)
    McpServer.start ()
  with
  | Stdlib.Exit ->
    exit 1
  | Sys.Break ->
    Printexc.print_backtrace stderr;
    Logs.error "MCP server interrupted";
    exit 130
  | exn ->
    Printexc.print_backtrace stderr;
    Logs.error "MCP server error: %s" (Printexc.to_string exn);
    exit 1
