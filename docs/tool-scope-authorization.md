# Tool-scope authorization invariants

This document states the end-to-end authorization contract for provider
visibility, discovery, execution, aliases, MCP origin, credentials, and reload.
It does **not** equate a JSON `filter_map` (or any presentation-layer list
filter) with authorization.

## Single rule surface

Authorization for tools is **deny-wins over equivalence classes**
(`Tool_authz`):

1. If any name in the tool’s equivalence class (canonical identity + aliases)
   appears on a deny list, the whole class is denied.
2. Otherwise, if an allowlist is nonempty, the class is admitted when any
   equivalent name is listed.
3. Otherwise the class is admitted.

Presentation helpers that filter JSON for UI display are **not** authorization
boundaries.

## Frozen catalog

Per turn / executable work unit, Clawq freezes an immutable `Tool_catalog`
before provider request construction:

| Field | Meaning |
|-------|---------|
| Canonical identity | Primary tool name |
| Aliases | Legacy / alternate names in the same class |
| Origin | builtin / skill / mcp:server |
| Schema revision | Hash of the frozen parameters schema |
| Deferred flag | Whether the tool is deferred for discovery |
| Access revision | Snapshot / config hash bound at freeze |

An in-flight turn cannot gain or replace tools after config or MCP reload.
Simultaneous Rooms hold distinct catalogs.

## Provider visibility

Provider payloads (`Tool_catalog` / `Tool_discovery` / vendor adapters) may
only include **authorized** tools from the frozen catalog:

- **OpenAI**: unselected deferred schemas are not dumped; portable
  `search_tools` / `inspect_tool` / `call_tool` are used when deferred tools
  exist.
- **Anthropic**: may include authorized deferred definitions only; denied tools
  never appear.
- **Generic / other**: portable discovery path.

Unauthorized names, descriptions, schemas, aliases, and counts must not reach a
provider.

## Discovery

Portable discovery (`Tool_discovery`):

- `search_tools` returns at most five short **authorized** results.
- `inspect_tool` returns one selected schema after authorization.
- `call_tool` reauthorizes the canonical identity without leaking the catalog.

Native deferred discovery is an adapter over the same frozen authorized set,
not a separate trust boundary.

## Execution

Invocation must re-check:

1. Room / profile / snapshot policy (`Tool_authz`, access snapshot).
2. Frozen catalog membership (`Tool_catalog.authorize_invoke`).
3. For MCP tools: current published server/tool revision
   (`Mcp_catalog.can_invoke` / `Mcp_registry_publish.revalidate_invoke`),
   refusing quarantined, unavailable, removed, or replaced revisions.

## MCP origin, credentials, reload

- MCP identity is `(server, remote_name)` plus revision (`Mcp_catalog`).
- `tools/list` pagination must fully drain; incomplete drains fail closed.
- `list_changed` quarantines the affected server/revision for **new** turns
  before relist; failed/malformed/timed-out relists leave the server
  unavailable until successful retry or repair.
- Removed tools are never discoverable after a successful relist that omits them.
- Credentials and clients are Room-scoped (`Mcp_room_scope`): HTTP credentials
  leased per call; credential-bearing stdio clients are scope-keyed. Rooms
  cannot see or use each other’s servers or credentials.
- Local registry replacement is transactional (`Mcp_registry_publish`):
  validate before atomic publish; on failure retain the previous validated
  state. Active Rooms refresh on their **next** turn after publish.

## Conformance

See `test/test_tool_scope_conformance.ml`: the same allow/deny lists must yield
identical allow/deny outcomes for provider payload membership, discovery
search/inspect, and invoke authorization.
