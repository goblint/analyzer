# MCP Server Architecture

This document describes the architecture and implementation details of the Goblint MCP Server.

## Overview

The MCP (Model Context Protocol) server provides a standardized way for LLMs and other clients to interact with Goblint's static analysis capabilities. It implements the MCP specification version 2024-11-05.

## Architecture

```
┌─────────────────┐
│  MCP Client     │ (e.g., Claude Desktop, custom tools)
│  (LLM/Tool)     │
└────────┬────────┘
         │ JSON-RPC 2.0 over stdin/stdout
         ▼
┌─────────────────┐
│  mcpServer.ml   │ MCP Protocol Layer
│  - Tool defs    │ - Handles MCP initialize/tools/call
│  - Request      │ - Calls Goblint functionality directly
│    handling     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Goblint Core   │ Analysis Engine
│  - Maingoblint  │ - Preprocessing and analysis
│  - CIL          │ - Configuration
│  - Solvers      │ - Query APIs
│  - Analyses     │
└─────────────────┘
```

## Key Components

### serverUtil.ml

Shared utilities for both JSON-RPC and MCP server implementations:

- **ArgWrapper Creation**: Builds ARG access with node lookup capabilities
- **InvariantParser Creation**: Creates parsers for C expression evaluation
- **NodeLocator Creation**: Provides CFG node lookup by source location
- **is_server_node**: Filters synthetic nodes for server operations

This module eliminates code duplication between different server implementations.

### mcpServer.ml

The main MCP server implementation:

- **Protocol Types**: Defines MCP-specific types (ToolInput, ToolCall, ToolResult)
- **Tool Definitions**: Specifies available tools with JSON schemas
- **Request Handlers**: Processes MCP requests and calls Goblint functions directly
- **I/O Loop**: Reads JSON-RPC from stdin, writes to stdout
- **State Management**: Manages CIL file and reuses ServerUtil components for ARG, invariant parsing, and node location

### goblint_mcp_server.ml

Simple executable entry point that:
1. Parses command-line arguments
2. Initializes Goblint
3. Starts the MCP server loop

## Protocol Flow

1. **Initialize**: Client sends `initialize` with capabilities
2. **Tools Discovery**: Client calls `tools/list` to get available tools
3. **Tool Execution**: Client calls `tools/call` with tool name and arguments
4. **Response**: Server returns results as MCP ToolResult objects

## Tool Implementation Pattern

Each tool follows this pattern:

```ocaml
| "tool_name" ->
  (* 1. Extract and validate arguments *)
  let arg = Yojson.Safe.Util.member "arg" args |> to_type in
  
  (* 2. Configure Goblint if needed *)
  GobConfig.set_* "option" value;
  
  (* 3. Execute operation directly *)
  Maingoblint.do_analyze None file;
  
  (* 4. Return result *)
  ToolResult.make_text "Success message"
```

## Configuration Handling

The `configure` tool converts MCP JSON values to Goblint config format:
- `string` → `"value"` (with quotes)
- `bool` → `true` / `false`
- `int` → `123`
- `float` → `123.45`
- Complex types → JSON string

## Error Handling

- JSON parse errors: Logged with input context, server continues
- Tool errors: Returned as `ToolResult` with `isError: true`
- Analysis errors: Caught and converted to error results
- EOF on stdin: Clean shutdown with log message

## Future Extensions

To add new tools:

1. Add tool definition to `tools` list with:
   - Name
   - Description
   - Input JSON schema
   
2. Add case to `handle_tools_call`:
   ```ocaml
   | "new_tool" ->
     (* implementation *)
     ToolResult.make_text "result"
   ```

3. Update documentation in `docs/user-guide/mcp-server.md`

## Testing

Use `scripts/mcp-test.py` for basic protocol testing:
```bash
./scripts/mcp-test.py path/to/test.c
```

For production testing, integrate with actual MCP clients like Claude Desktop.

## Related Modules

- **ServerUtil** (src/util/serverUtil.ml): Shared server utilities for ARG access, invariant parsing, and node location
- **Server** (src/util/server.ml): Existing JSON-RPC server (also uses ServerUtil)
- **Maingoblint**: Entry points for preprocessing, parsing, and analysis execution
- **GobConfig**: Configuration management
- **Messages**: Analysis warning/error collection
- **Cilfacade**: CIL interface and variable information
- **ArgTools**: ARG (Abstract Reachability Graph) infrastructure
- **WitnessUtil**: Invariant parsing and location lookup infrastructure

## Dependencies

- `jsonrpc` (>= 1.12): JSON-RPC 2.0 protocol implementation
- `yojson`: JSON parsing and generation
- `ppx_deriving_yojson`: Automatic JSON serialization

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [Goblint Server Documentation](../docs/user-guide/inspecting.md)
- [JSON-RPC 2.0 Spec](https://www.jsonrpc.org/specification)
