#!/usr/bin/env python3
"""
Simple test script for the Goblint MCP Server.

This script demonstrates how to interact with the goblint-mcp-server
by sending JSON-RPC 2.0 requests and receiving responses.
"""

import json
import subprocess
import sys

def send_request(process, request):
    """Send a JSON-RPC request to the server and get the response."""
    request_str = json.dumps(request) + "\n"
    process.stdin.write(request_str)
    process.stdin.flush()
    
    response_str = process.stdout.readline()
    return json.loads(response_str)

def main():
    # Start the MCP server
    print("Starting goblint-mcp-server...")
    process = subprocess.Popen(
        ["goblint-mcp-server"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    try:
        # 1. Initialize
        print("\n1. Initializing connection...")
        response = send_request(process, {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {
                    "name": "goblint-mcp-test",
                    "version": "1.0.0"
                }
            }
        })
        print(f"Response: {json.dumps(response, indent=2)}")
        
        # 2. Send initialized notification
        print("\n2. Sending initialized notification...")
        send_request(process, {
            "jsonrpc": "2.0",
            "method": "initialized"
        })
        
        # 3. List available tools
        print("\n3. Listing available tools...")
        response = send_request(process, {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list"
        })
        print(f"Available tools:")
        for tool in response.get("result", {}).get("tools", []):
            print(f"  - {tool['name']}: {tool.get('description', 'No description')}")
        
        # 4. Example: Analyze a file (if provided)
        if len(sys.argv) > 1:
            test_file = sys.argv[1]
            print(f"\n4. Analyzing file: {test_file}")
            response = send_request(process, {
                "jsonrpc": "2.0",
                "id": 3,
                "method": "tools/call",
                "params": {
                    "name": "analyze_file",
                    "arguments": {
                        "file": test_file
                    }
                }
            })
            print(f"Analysis result: {json.dumps(response, indent=2)}")
            
            # 5. Get messages
            print("\n5. Getting analysis messages...")
            response = send_request(process, {
                "jsonrpc": "2.0",
                "id": 4,
                "method": "tools/call",
                "params": {
                    "name": "get_messages"
                }
            })
            print(f"Messages: {json.dumps(response, indent=2)}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    finally:
        process.terminate()
        process.wait()
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
