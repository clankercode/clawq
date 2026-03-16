# Agent Client Protocol (ACP) - Comprehensive Reference

> Source: https://agentclientprotocol.com and https://github.com/agentclientprotocol/agent-client-protocol
>
> Protocol Version: 1 (integer, incremented only for breaking changes)
>
> Schema version sourced: 2026-03-15

## Overview

The Agent Client Protocol (ACP) standardizes communication between **code editors** (Clients) and **coding agents** (Agents). It is analogous to the Language Server Protocol (LSP) but for AI coding agents instead of language servers.

- **Created by**: Zed Industries, with involvement from JetBrains and others
- **License**: Apache 2.0
- **Encoding**: JSON-RPC 2.0 over various transports
- **Text format**: Markdown (default for user-readable content)
- **Reuses MCP types**: ContentBlock, Annotations, ResourceLink, etc.

### Problem Solved

Each editor must build custom integrations for every agent, and agents must implement editor-specific APIs. ACP eliminates this N×M integration problem by providing a single standardized protocol.

### Official SDKs

- **Rust**: `agent-client-protocol` crate
- **Python**: `@agentclientprotocol/python-sdk`
- **TypeScript**: `@agentclientprotocol/sdk`
- **Kotlin**: `acp-kotlin`
- **Java**: `java-sdk`

---

## Architecture

### Roles

**Client** (code editor/IDE):
- Initiates agent connections on-demand
- Manages user sessions and concurrent conversations
- Controls tool call permissions
- Hosts local files and MCP server access
- Can export its own MCP-based tools via proxy

**Agent** (AI sub-process):
- Runs as subprocess spawned by Client
- Processes user prompts and makes tool requests
- Connects directly to MCP servers
- Requests permissions from the Client for actions

### Communication Model

Bidirectional JSON-RPC 2.0 with two message types:
- **Methods** (requests): Request-response pairs expecting result or error
- **Notifications**: One-way messages, no response expected

### Transport

All communication over stdin/stdout (primary, MUST support) with newline-delimited JSON-RPC messages. Streamable HTTP is in draft.

---

## Transport: stdio

- Client launches agent as subprocess
- Agent reads JSON-RPC from stdin, writes to stdout
- Messages delimited by newlines (`\n`), MUST NOT contain embedded newlines
- Agent MAY write to stderr for logging (Client MAY capture/ignore)
- Agent MUST NOT write non-ACP content to stdout
- Client MUST NOT write non-ACP content to agent's stdin

```
Client ──stdin──> Agent Process
Client <──stdout── Agent Process
Client <──stderr── Agent Process (optional logs)
```

### Streamable HTTP

Draft proposal in progress. Not yet specified.

### Custom Transports

Allowed. Must preserve JSON-RPC format and ACP lifecycle. Should document connection patterns.

---

## Protocol Flow

### Phase 1: Initialization

```
Client → Agent: initialize (protocolVersion, clientCapabilities, clientInfo)
Agent → Client: initialize response (protocolVersion, agentCapabilities, agentInfo, authMethods)
```

Optional: `Client → Agent: authenticate (methodId)` if required.

### Phase 2: Session Setup

Either:
- `Client → Agent: session/new (cwd, mcpServers)` → response with sessionId
- `Client → Agent: session/load (sessionId, cwd, mcpServers)` → replay conversation via session/update, then response

### Phase 3: Prompt Turn (repeating)

```
Client → Agent: session/prompt (sessionId, prompt[])
  Agent → Client: session/update (plan, agent_message_chunk, tool_call, etc.)
  Agent → Client: session/request_permission (if needed)
  Client → Agent: permission response
  Agent → Client: session/update (tool_call_update with status changes)
Agent → Client: session/prompt response (stopReason)
```

Optional: `Client → Agent: session/cancel (sessionId)` to interrupt.

---

## Methods Reference

### Agent Methods (Client → Agent)

| Method | Required | Description |
|--------|----------|-------------|
| `initialize` | YES | Negotiate protocol version and capabilities |
| `authenticate` | conditional | Authenticate with agent (if authMethods returned) |
| `session/new` | YES | Create new conversation session |
| `session/prompt` | YES | Send user message to agent |
| `session/cancel` | YES (notification) | Cancel ongoing prompt turn |
| `session/load` | optional | Resume existing session (requires `loadSession` capability) |
| `session/list` | optional | List known sessions (requires `sessionCapabilities.list`) |
| `session/set_mode` | optional | Switch agent operating mode |
| `session/set_config_option` | optional | Change session configuration |

### Client Methods (Agent → Client)

| Method | Required | Description |
|--------|----------|-------------|
| `session/request_permission` | YES | Request user authorization for tool call |
| `session/update` | YES (notification) | Stream session updates to client |
| `fs/read_text_file` | optional | Read file from client filesystem (requires `fs.readTextFile`) |
| `fs/write_text_file` | optional | Write file to client filesystem (requires `fs.writeTextFile`) |
| `terminal/create` | optional | Execute command in new terminal (requires `terminal`) |
| `terminal/output` | optional | Get terminal output and exit status |
| `terminal/wait_for_exit` | optional | Wait for terminal command to exit |
| `terminal/kill` | optional | Kill terminal command without releasing |
| `terminal/release` | optional | Release terminal and all resources |

---

## Initialization

### Initialize Request (Client → Agent)

```json
{
  "jsonrpc": "2.0",
  "id": 0,
  "method": "initialize",
  "params": {
    "protocolVersion": 1,
    "clientCapabilities": {
      "fs": {
        "readTextFile": true,
        "writeTextFile": true
      },
      "terminal": true
    },
    "clientInfo": {
      "name": "clawq",
      "title": "Clawq Agent Runtime",
      "version": "0.1.0"
    }
  }
}
```

### Initialize Response (Agent → Client)

```json
{
  "jsonrpc": "2.0",
  "id": 0,
  "result": {
    "protocolVersion": 1,
    "agentCapabilities": {
      "loadSession": true,
      "promptCapabilities": {
        "image": true,
        "audio": false,
        "embeddedContext": true
      },
      "mcpCapabilities": {
        "http": false,
        "sse": false
      },
      "sessionCapabilities": {
        "list": {}
      }
    },
    "agentInfo": {
      "name": "my-agent",
      "title": "My Agent",
      "version": "1.0.0"
    },
    "authMethods": []
  }
}
```

### Version Negotiation

- `protocolVersion` is a single integer (currently `1`)
- Client sends latest version it supports
- Agent responds with same version if supported, otherwise latest it supports
- If Client doesn't support Agent's version, it SHOULD close connection

### Client Capabilities

| Capability | Type | Description |
|-----------|------|-------------|
| `fs.readTextFile` | boolean | Client supports `fs/read_text_file` method |
| `fs.writeTextFile` | boolean | Client supports `fs/write_text_file` method |
| `terminal` | boolean | Client supports all `terminal/*` methods |

### Agent Capabilities

| Capability | Type | Description |
|-----------|------|-------------|
| `loadSession` | boolean | Agent supports `session/load` method |
| `promptCapabilities.image` | boolean | Prompts may include image content |
| `promptCapabilities.audio` | boolean | Prompts may include audio content |
| `promptCapabilities.embeddedContext` | boolean | Prompts may include embedded resources |
| `mcpCapabilities.http` | boolean | Agent supports MCP over HTTP |
| `mcpCapabilities.sse` | boolean | Agent supports MCP over SSE (deprecated by MCP) |
| `sessionCapabilities.list` | object | Agent supports `session/list` method |

### Authentication

If `authMethods` is non-empty in initialize response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "authenticate",
  "params": {
    "methodId": "api-key"
  }
}
```

Response: `{ "result": {} }` on success.

---

## Session Setup

### session/new (Client → Agent)

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "session/new",
  "params": {
    "cwd": "/home/user/project",
    "mcpServers": [
      {
        "name": "filesystem",
        "command": "/path/to/mcp-server",
        "args": ["--stdio"],
        "env": [{ "name": "API_KEY", "value": "secret123" }]
      }
    ]
  }
}
```

Response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "sessionId": "sess_abc123def456",
    "modes": { ... },
    "configOptions": [ ... ]
  }
}
```

### session/load (Client → Agent)

Requires `loadSession` capability. Agent replays conversation via `session/update` notifications, then responds.

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "session/load",
  "params": {
    "sessionId": "sess_789xyz",
    "cwd": "/home/user/project",
    "mcpServers": [...]
  }
}
```

### session/list (Client → Agent)

Requires `sessionCapabilities.list`. Returns paginated session list.

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "session/list",
  "params": {
    "cwd": "/home/user/project",
    "cursor": "eyJwYWdlIjogMn0="
  }
}
```

Response includes `sessions[]` (SessionInfo with sessionId, cwd, title, updatedAt) and optional `nextCursor`.

### Working Directory

- `cwd` MUST be an absolute path
- MUST be used for the session regardless of where agent was spawned
- SHOULD serve as boundary for tool operations

### MCP Servers

Passed in session/new and session/load. Three transport types:

1. **Stdio** (all agents MUST support): `{ name, command, args, env }`
2. **HTTP** (requires `mcpCapabilities.http`): `{ type: "http", name, url, headers }`
3. **SSE** (requires `mcpCapabilities.sse`, deprecated): `{ type: "sse", name, url, headers }`

---

## Prompt Turn

### session/prompt (Client → Agent)

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "session/prompt",
  "params": {
    "sessionId": "sess_abc123def456",
    "prompt": [
      { "type": "text", "text": "Can you analyze this code?" },
      {
        "type": "resource",
        "resource": {
          "uri": "file:///home/user/project/main.py",
          "mimeType": "text/x-python",
          "text": "def process_data(items): ..."
        }
      }
    ]
  }
}
```

### session/prompt Response

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "stopReason": "end_turn"
  }
}
```

### Stop Reasons

| Reason | Description |
|--------|-------------|
| `end_turn` | LLM finished responding without requesting more tools |
| `max_tokens` | Maximum token limit reached |
| `max_turn_requests` | Maximum model requests in single turn exceeded |
| `refusal` | Agent refuses to continue |
| `cancelled` | Client cancelled the turn |

### session/update Notifications (Agent → Client)

Discriminated by `sessionUpdate` field:

| sessionUpdate | Description |
|---------------|-------------|
| `user_message_chunk` | Streamed user message content (during session/load replay) |
| `agent_message_chunk` | Streamed agent response content |
| `thought_message_chunk` | Streamed agent thought/reasoning content |
| `tool_call` | New tool call reported |
| `tool_call_update` | Tool call status/content update |
| `plan` | Execution plan with entries |
| `available_commands_update` | Slash commands available |
| `current_mode_update` | Mode change notification |
| `config_option_update` | Config options changed |
| `session_info_update` | Session metadata (title, etc.) changed |

### Agent Message Chunk Example

```json
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123def456",
    "update": {
      "sessionUpdate": "agent_message_chunk",
      "content": {
        "type": "text",
        "text": "I'll analyze your code for potential issues..."
      }
    }
  }
}
```

### Cancellation

Client sends `session/cancel` notification (no response expected):

```json
{
  "jsonrpc": "2.0",
  "method": "session/cancel",
  "params": { "sessionId": "sess_abc123def456" }
}
```

Agent SHOULD: stop LLM requests, abort tool calls, send pending updates, respond to original session/prompt with `stopReason: "cancelled"`.

Client MUST respond to pending `session/request_permission` with `cancelled` outcome.

---

## Tool Calls

### Creating a Tool Call

```json
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123def456",
    "update": {
      "sessionUpdate": "tool_call",
      "toolCallId": "call_001",
      "title": "Reading configuration file",
      "kind": "read",
      "status": "pending",
      "content": [],
      "locations": [],
      "rawInput": {},
      "rawOutput": {}
    }
  }
}
```

### Tool Kinds

| Kind | Description |
|------|-------------|
| `read` | Reading files or data |
| `edit` | Modifying files or content |
| `delete` | Removing files or data |
| `move` | Moving or renaming files |
| `search` | Searching for information |
| `execute` | Running commands or code |
| `think` | Internal reasoning or planning |
| `fetch` | Retrieving external data |
| `other` | Other tool types (default) |

### Tool Call Status

| Status | Description |
|--------|-------------|
| `pending` | Not started (streaming input or awaiting approval) |
| `in_progress` | Currently running |
| `completed` | Completed successfully |
| `failed` | Failed with error |

### Updating a Tool Call

```json
{
  "jsonrpc": "2.0",
  "method": "session/update",
  "params": {
    "sessionId": "sess_abc123def456",
    "update": {
      "sessionUpdate": "tool_call_update",
      "toolCallId": "call_001",
      "status": "completed",
      "content": [
        {
          "type": "content",
          "content": { "type": "text", "text": "Analysis complete." }
        }
      ]
    }
  }
}
```

### Tool Call Content Types

1. **Regular content**: `{ "type": "content", "content": <ContentBlock> }`
2. **Diff**: `{ "type": "diff", "path": "/abs/path", "oldText": "...", "newText": "..." }`
3. **Terminal**: `{ "type": "terminal", "terminalId": "term_xyz789" }`

### Tool Call Locations

```json
{
  "path": "/home/user/project/src/main.py",
  "line": 42
}
```

### Requesting Permission

Agent → Client request:

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "session/request_permission",
  "params": {
    "sessionId": "sess_abc123def456",
    "toolCall": {
      "toolCallId": "call_001",
      "title": "...",
      "kind": "execute",
      "status": "pending"
    },
    "options": [
      { "optionId": "allow-once", "name": "Allow once", "kind": "allow_once" },
      { "optionId": "reject-once", "name": "Reject", "kind": "reject_once" }
    ]
  }
}
```

Client response:

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "outcome": {
      "outcome": "selected",
      "optionId": "allow-once"
    }
  }
}
```

### Permission Option Kinds

| Kind | Description |
|------|-------------|
| `allow_once` | Allow this time only |
| `allow_always` | Allow and remember |
| `reject_once` | Reject this time only |
| `reject_always` | Reject and remember |

---

## File System

### fs/read_text_file (Agent → Client)

Requires `fs.readTextFile` capability.

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "fs/read_text_file",
  "params": {
    "sessionId": "sess_abc123def456",
    "path": "/home/user/project/src/main.py",
    "line": 10,
    "limit": 50
  }
}
```

Response: `{ "result": { "content": "file contents..." } }`

### fs/write_text_file (Agent → Client)

Requires `fs.writeTextFile` capability.

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "fs/write_text_file",
  "params": {
    "sessionId": "sess_abc123def456",
    "path": "/home/user/project/config.json",
    "content": "file contents..."
  }
}
```

Response: `{ "result": null }`

Client MUST create file if it doesn't exist.

---

## Terminals

Requires `terminal` client capability for all terminal/* methods.

### terminal/create (Agent → Client)

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "terminal/create",
  "params": {
    "sessionId": "sess_abc123def456",
    "command": "npm",
    "args": ["test", "--coverage"],
    "env": [{ "name": "NODE_ENV", "value": "test" }],
    "cwd": "/home/user/project",
    "outputByteLimit": 1048576
  }
}
```

Response: `{ "result": { "terminalId": "term_xyz789" } }` (returns immediately)

### terminal/output (Agent → Client)

Returns current output without waiting:

```json
{
  "result": {
    "output": "Running tests...\n✓ All tests passed\n",
    "truncated": false,
    "exitStatus": { "exitCode": 0, "signal": null }
  }
}
```

### terminal/wait_for_exit (Agent → Client)

Blocks until command exits. Response: `{ "result": { "exitCode": 0, "signal": null } }`

### terminal/kill (Agent → Client)

Kills command but keeps terminal valid. Agent MUST still call terminal/release.

### terminal/release (Agent → Client)

Kills command if running, releases all resources. Terminal ID becomes invalid.

### Timeout Pattern

1. `terminal/create` → get terminalId
2. Start timer
3. Race: timer vs `terminal/wait_for_exit`
4. If timer wins: `terminal/kill` → `terminal/output` → use output
5. `terminal/release` when done

---

## Content Blocks

Shared with MCP. Discriminated by `type` field.

### Text (baseline, all agents MUST support)

```json
{ "type": "text", "text": "Hello, world!" }
```

### Image (requires `promptCapabilities.image`)

```json
{ "type": "image", "mimeType": "image/png", "data": "base64..." }
```

### Audio (requires `promptCapabilities.audio`)

```json
{ "type": "audio", "mimeType": "audio/wav", "data": "base64..." }
```

### Embedded Resource (requires `promptCapabilities.embeddedContext`)

```json
{
  "type": "resource",
  "resource": {
    "uri": "file:///path/to/file.py",
    "mimeType": "text/x-python",
    "text": "def hello(): ..."
  }
}
```

### Resource Link (baseline, all agents MUST support in prompts)

```json
{
  "type": "resource_link",
  "uri": "file:///path/to/doc.pdf",
  "name": "doc.pdf",
  "mimeType": "application/pdf",
  "size": 1024000
}
```

### Annotations (optional on all content)

```json
{
  "audience": ["user"],
  "lastModified": "2025-10-29T14:22:15Z",
  "priority": 0.8
}
```

---

## Agent Plan

Plans are execution strategies sent via `session/update` with `sessionUpdate: "plan"`.

```json
{
  "sessionUpdate": "plan",
  "entries": [
    { "content": "Analyze codebase", "priority": "high", "status": "completed" },
    { "content": "Implement changes", "priority": "high", "status": "in_progress" },
    { "content": "Write tests", "priority": "medium", "status": "pending" }
  ]
}
```

### PlanEntry

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `content` | string | YES | Human-readable task description |
| `priority` | enum | YES | `high`, `medium`, `low` |
| `status` | enum | YES | `pending`, `in_progress`, `completed` |

Agent MUST send complete plan in each update (Client replaces entirely).

---

## Slash Commands

Agents advertise commands via `available_commands_update`:

```json
{
  "sessionUpdate": "available_commands_update",
  "availableCommands": [
    { "name": "web", "description": "Search the web", "input": { "hint": "query" } },
    { "name": "test", "description": "Run tests" }
  ]
}
```

Commands are invoked as regular prompts: `{ "type": "text", "text": "/web agent client protocol" }`.

---

## Session Modes (deprecated, use Config Options)

Agents MAY return modes in session/new response:

```json
{
  "modes": {
    "currentModeId": "ask",
    "availableModes": [
      { "id": "ask", "name": "Ask", "description": "Request permission before changes" },
      { "id": "code", "name": "Code", "description": "Full tool access" }
    ]
  }
}
```

Change via `session/set_mode` or agent notification `current_mode_update`.

---

## Session Config Options (preferred over modes)

```json
{
  "configOptions": [
    {
      "id": "mode",
      "name": "Session Mode",
      "category": "mode",
      "type": "select",
      "currentValue": "ask",
      "options": [
        { "value": "ask", "name": "Ask" },
        { "value": "code", "name": "Code" }
      ]
    },
    {
      "id": "model",
      "name": "Model",
      "category": "model",
      "type": "select",
      "currentValue": "model-1",
      "options": [
        { "value": "model-1", "name": "Model 1" },
        { "value": "model-2", "name": "Model 2" }
      ]
    }
  ]
}
```

### Categories

| Category | Description |
|----------|-------------|
| `mode` | Session mode selector |
| `model` | Model selector |
| `thought_level` | Reasoning level selector |
| `_*` | Custom (prefix with `_`) |

Change via `session/set_config_option`. Response returns complete config state.

---

## Extensibility

### _meta Field

All types include `_meta: { [key: string]: unknown }` for custom data.

Reserved root keys for W3C trace context: `traceparent`, `tracestate`, `baggage`.

### Extension Methods

Method names starting with `_` are reserved for extensions:
- Requests: include `id`, expect response
- Notifications: omit `id`, one-way

Example: `_zed.dev/workspace/buffers`

Unrecognized requests: respond with `-32601` (Method not found).
Unrecognized notifications: SHOULD be ignored.

### Custom Capabilities

Advertise via `_meta` in capability objects during initialization.

---

## Error Handling

Standard JSON-RPC 2.0 error codes:

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid request | Not a valid Request object |
| -32601 | Method not found | Method doesn't exist |
| -32602 | Invalid params | Invalid parameters |
| -32603 | Internal error | Internal JSON-RPC error |

---

## Protocol Rules

1. All file paths MUST be absolute
2. Line numbers are 1-based
3. Protocol version is a single integer
4. New capabilities are NOT breaking changes
5. Omitted capabilities = UNSUPPORTED
6. MUST NOT add custom root fields to spec types (use `_meta`)
7. Extension methods MUST start with `_`

---

## Complete meta.json (Method Registry)

```json
{
  "agentMethods": {
    "authenticate": "authenticate",
    "initialize": "initialize",
    "session_cancel": "session/cancel",
    "session_list": "session/list",
    "session_load": "session/load",
    "session_new": "session/new",
    "session_prompt": "session/prompt",
    "session_set_config_option": "session/set_config_option",
    "session_set_mode": "session/set_mode"
  },
  "clientMethods": {
    "fs_read_text_file": "fs/read_text_file",
    "fs_write_text_file": "fs/write_text_file",
    "session_request_permission": "session/request_permission",
    "session_update": "session/update",
    "terminal_create": "terminal/create",
    "terminal_kill": "terminal/kill",
    "terminal_output": "terminal/output",
    "terminal_release": "terminal/release",
    "terminal_wait_for_exit": "terminal/wait_for_exit"
  },
  "version": 1
}
```

---

## Schema Type Definitions Summary

### Core Types

| Type | Description |
|------|-------------|
| `SessionId` | string - unique session identifier |
| `ProtocolVersion` | uint16 integer (currently 1) |
| `RequestId` | integer or string - JSON-RPC request ID |
| `Role` | enum: `"assistant"` or `"user"` |
| `StopReason` | enum: `end_turn`, `max_tokens`, `max_turn_requests`, `refusal`, `cancelled` |
| `ToolCallId` | string - unique tool call ID within session |
| `ToolCallStatus` | enum: `pending`, `in_progress`, `completed`, `failed` |
| `ToolKind` | enum: `read`, `edit`, `delete`, `move`, `search`, `execute`, `think`, `fetch`, `other` |
| `PermissionOptionId` | string |
| `PermissionOptionKind` | enum: `allow_once`, `allow_always`, `reject_once`, `reject_always` |
| `PlanEntryStatus` | enum: `pending`, `in_progress`, `completed` |
| `PlanEntryPriority` | enum: `high`, `medium`, `low` |
| `SessionModeId` | string |
| `SessionConfigId` | string |
| `SessionConfigValueId` | string |
| `SessionConfigGroupId` | string |
| `SessionConfigOptionCategory` | string (reserved: `mode`, `model`, `thought_level`) |

### Composite Types

| Type | Key Fields |
|------|------------|
| `Implementation` | `name`, `title?`, `version?` |
| `ClientCapabilities` | `fs: FileSystemCapabilities`, `terminal: bool` |
| `AgentCapabilities` | `loadSession`, `promptCapabilities`, `mcpCapabilities`, `sessionCapabilities` |
| `FileSystemCapabilities` | `readTextFile: bool`, `writeTextFile: bool` |
| `PromptCapabilities` | `image: bool`, `audio: bool`, `embeddedContext: bool` |
| `McpCapabilities` | `http: bool`, `sse: bool` |
| `SessionCapabilities` | `list: SessionListCapabilities?` |
| `ContentBlock` | discriminated by `type`: text, image, audio, resource, resource_link |
| `ContentChunk` | `content: ContentBlock` |
| `ToolCall` | `toolCallId`, `title`, `kind`, `status`, `content[]`, `locations[]`, `rawInput`, `rawOutput` |
| `ToolCallUpdate` | `toolCallId`, `status?`, `title?`, `content?`, `locations?`, `rawInput?`, `rawOutput?` |
| `ToolCallContent` | discriminated by `type`: content, diff, terminal |
| `Diff` | `path`, `oldText?`, `newText` |
| `Terminal` | `terminalId` |
| `Plan` | `entries: PlanEntry[]` |
| `PlanEntry` | `content`, `priority`, `status` |
| `SessionMode` | `id`, `name`, `description?` |
| `SessionModeState` | `currentModeId`, `availableModes[]` |
| `SessionConfigOption` | discriminated by `type`: select |
| `SessionConfigSelect` | `id`, `name`, `description?`, `category?`, `currentValue`, `options[]` |
| `AvailableCommand` | `name`, `description`, `input?` |
| `SessionInfo` | `sessionId`, `cwd`, `title?`, `updatedAt?` |
| `McpServer` | discriminated: stdio (name, command, args, env), http (name, url, headers), sse |
| `PermissionOption` | `optionId`, `name`, `kind` |
| `RequestPermissionOutcome` | discriminated: `cancelled` or `selected` with `optionId` |
| `EnvVariable` | `name`, `value` |
| `ToolCallLocation` | `path`, `line?` |

### SessionUpdate Variants

Discriminated by `sessionUpdate` field:

| Variant | Type | Description |
|---------|------|-------------|
| `user_message_chunk` | ContentChunk | User message content |
| `agent_message_chunk` | ContentChunk | Agent response content |
| `thought_message_chunk` | ContentChunk | Agent reasoning/thought |
| `tool_call` | ToolCall | New tool call |
| `tool_call_update` | ToolCallUpdate | Tool call progress |
| `plan` | Plan | Execution plan |
| `available_commands_update` | AvailableCommandsUpdate | Slash commands |
| `current_mode_update` | CurrentModeUpdate | Mode change |
| `config_option_update` | ConfigOptionUpdate | Config options changed |
| `session_info_update` | SessionInfoUpdate | Session metadata |

---

## Ecosystem

### Implementing Agents (40+)

Claude Code, Gemini CLI, GitHub Copilot, Cursor, Cline, Junie (JetBrains), Zed adapters, Docker cagent, OpenHands, Goose, Qwen Code, and many more.

### Implementing Clients (30+)

VS Code, Zed, JetBrains IDEs, Neovim, Emacs, Obsidian, Discord/Slack/Telegram bots, iOS/Android apps, Jupyter kernels, LangChain, LlamaIndex, and many more.

### Official Repository

https://github.com/agentclientprotocol/agent-client-protocol

Contains:
- `schema/schema.json` - Complete JSON Schema (3597 lines)
- `schema/meta.json` - Method registry
- `schema/schema.unstable.json` - Unstable/draft features
- `src/` - Rust reference implementation
- `docs/protocol/` - Protocol specification docs
