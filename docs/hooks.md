# Lifecycle hooks

Lifecycle hooks run a command or make an HTTP `POST` at well-defined points in
an agent turn.  A command hook receives one JSON object on standard input.  It
must reserve standard output for a single JSON response; diagnostics and block
reasons belong on standard error.

Hooks run with `/bin/sh -c`. Treat hook configuration as trusted local code:
the hook has the same access as the Clawq process.

## Configure hooks

Use the `hooks` member in `config.json` (the wrapped, Claude Code-compatible
form):

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash|Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "./docs/examples/hooks/block-dangerous.sh",
            "timeout": 10,
            "async": false
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          { "type": "command", "command": "./docs/examples/hooks/auto-format.sh" }
        ]
      }
    ]
  }
}
```

For a project-local `.clawq/hooks.json`, put event names at the root instead:

```json
{
  "PreToolUse": [
    {
      "matcher": "Bash",
      "hooks": [
        { "type": "command", "command": "./docs/examples/hooks/block-dangerous.sh" }
      ]
    }
  ]
}
```

Clawq also reads the same root-event format from the profile file at
`$CLAWQ_HOME/hooks.json` (normally `~/.clawq/hooks.json`). Sources are
additive and run in this order: project file, profile file, then the `hooks`
section in `config.json`.

Use `clawq hooks list` to show the `config.json` hooks, `clawq hooks validate`
to validate them, and `clawq hooks events` to print all supported event names.

The parser accepts these handler fields:

| Field | Meaning |
| --- | --- |
| `type` | `command` (default) or `http` |
| `command` | Shell command for `command`; URL for `http` (an HTTP hook may use `url` instead) |
| `timeout` | Seconds, default `30`, capped at `300`; non-positive values use the default |
| `async` | `true` starts the handler without waiting for its result |
| `headers` | Object of extra HTTP headers |
| `allowedEnvVars` | HTTP-header environment variables that may be interpolated as `$VARNAME` |

Each event holds an array of entries. An entry has a `hooks` array and an
optional `matcher`. For tool events, a matcher is an exact tool name, `*`, or
pipe-separated exact names such as `Bash|Write|Edit`. Despite some older
examples calling it a regex, regular expressions are not evaluated. A missing,
empty, or `*` matcher matches every tool. Matchers do not filter non-tool
events.

`http` hooks receive the same JSON body as command hooks. A `2xx` response is
success; every other response is an ordinary hook error. HTTP headers always
include `Content-Type: application/json`; only names in `allowedEnvVars` may be
expanded in configured header values.

## Events and payloads

Every payload is a JSON object. `session_id` and `cwd` are always strings;
`workspace` is supplied by normal Clawq dispatches. `tool_input` is the parsed
tool-argument JSON when possible, otherwise a JSON string. `tool_response` is
currently a JSON string containing the displayed tool result.

| Event | When it runs | Payload |
| --- | --- | --- |
| `PreToolUse` | Immediately before a validated tool is invoked | tool payload |
| `PostToolUse` | After a tool result that does not begin with `Error:` | tool payload with response |
| `PostToolUseFailure` | After a tool result that begins with `Error:` | tool payload with response |
| `UserPromptSubmit` | After channel metadata is applied to a user prompt, before the agent turn | prompt payload |
| `Stop` | After a normal agent turn completes | session payload |
| `SessionStart` | At the start of a session turn | session payload |
| `SessionEnd` | Session lifecycle-close event | session payload |
| `PreCompact` | Before automatic history compaction | session payload, `reason: "auto"` |
| `PostCompact` | After automatic history compaction | session payload, `reason: "auto"` |
| `OnError` | When a session turn raises an exception | error payload |

The exact schemas are:

```json
// tool payload (PreToolUse)
{
  "tool_name": "Bash",
  "tool_input": { "command": "make test" },
  "session_id": "telegram:123",
  "cwd": "/work/repo",
  "workspace": "/work/repo"
}
```

```json
// tool payload with response (PostToolUse / PostToolUseFailure)
{
  "tool_name": "Write",
  "tool_input": { "file_path": "README.md", "content": "..." },
  "tool_response": "Wrote README.md",
  "session_id": "telegram:123",
  "cwd": "/work/repo",
  "workspace": "/work/repo"
}
```

```json
// prompt payload (UserPromptSubmit)
{
  "prompt": "Please run the test suite.",
  "session_id": "telegram:123",
  "cwd": "/work/repo",
  "workspace": "/work/repo"
}
```

```json
// session payload (SessionStart, Stop, SessionEnd)
{
  "session_id": "telegram:123",
  "cwd": "/work/repo",
  "workspace": "/work/repo"
}

// compaction variants add a reason
{
  "session_id": "compacted",
  "cwd": "/work/repo",
  "workspace": "/work/repo",
  "reason": "auto"
}
```

```json
// error payload (OnError)
{
  "error_type": "agent_turn",
  "error_message": "Failure(\"provider unavailable\")",
  "session_id": "telegram:123",
  "cwd": "/work/repo",
  "workspace": "/work/repo"
}
```

Compaction uses the current session id when one is available; direct internal
compaction without a session context uses a descriptive id such as
`42 messages being compacted`. `OnError` identifies a failed agent turn with
`error_type: "agent_turn"` and carries the exception text in `error_message`.
`SessionEnd` is the lifecycle-close event; use it for cleanup that belongs to
the end of a session.

## Results and exit status

Exit status is evaluated for synchronous handlers in configuration order:

- `0` succeeds. Empty output is normal. If stdout is JSON, Clawq reads the
  response fields below.
- `2` blocks and uses stderr as the reason (or a JSON reason when stderr is
  empty).
- Any other status is an error: stderr is logged and execution continues.
- A timeout is an error and execution continues. Async hooks are
  fire-and-forget, so their output, exit code, and timeout cannot affect the
  current operation.

Only `PreToolUse` currently consumes a blocking result to prevent a tool call.
The hook model marks `UserPromptSubmit`, `Stop`, and `PreCompact` as
block-capable too, but their current dispatch call sites intentionally discard
the result. Post events, session start/end, post-compaction, and error events
are observational.

A successful hook may emit one of these JSON response shapes:

```json
{ "decision": "block", "reason": "Do not delete production data." }
{ "decision": "ask", "reason": "Confirm this migration." }
{ "decision": "allow", "additionalContext": "Run the focused test afterwards." }
```

For Claude Code-style permission output, use:

```json
{
  "additionalContext": "A repository policy applies.",
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "Protected branch",
    "updatedInput": { "command": "git status --short" }
  }
}
```

Clawq recognizes `allow`, `ask`, and `deny` in
`hookSpecificOutput.permissionDecision`; `deny` blocks. It injects the
top-level or hook-specific `additionalContext` into agent context.
`updatedInput` is used only by `PreToolUse`, replacing the parsed tool
arguments. `hookEventName` is accepted for Claude compatibility but is not
required by Clawq.

## Environment

Command hooks inherit Clawq's process environment plus:

| Variable | Value |
| --- | --- |
| `CLAWQ_PROJECT_DIR` | Effective current working directory (`cwd`) |
| `CLAWQ_WORKSPACE` | Configured workspace |
| `CLAWQ_HOOK_VERSION` | `1.0` |
| `CLAWQ_SESSION_ID` | Current session id when the dispatch has one |
| `CLAWQ_TOOL_NAME` | Current tool name for a tool-event dispatch |

Session identity and tool identity are also provided in the stdin JSON as
`session_id` and `tool_name`. The session and tool environment variables are
contextual conveniences, so portable scripts should use the payload when they
need an authoritative event-specific value.

## Examples

The companion scripts are POSIX `sh` and deliberately use no JSON dependency:

- [auto-format.sh](examples/hooks/auto-format.sh) formats recognized files
  after `Write` or `Edit`.
- [block-dangerous.sh](examples/hooks/block-dangerous.sh) blocks a small,
  explicit set of destructive shell commands.
- [transcript-backup.sh](examples/hooks/transcript-backup.sh) is a
  `PreCompact` hook that stores the event payload under
  `.clawq/hook-transcripts/`.
- [task-enforcement.sh](examples/hooks/task-enforcement.sh) is a `Stop` hook
  that requires `.clawq/current-task` to contain `done`, `complete`, or
  `status=done` before the turn may stop.

For example, wire transcript backups before compaction and enforce task
completion when the agent stops:

```json
{
  "hooks": {
    "PreCompact": [{ "hooks": [{ "command": "./docs/examples/hooks/transcript-backup.sh" }] }],
    "Stop": [{ "hooks": [{ "command": "./docs/examples/hooks/task-enforcement.sh" }] }]
  }
}
```

## Claude Code and Codex compatibility

The wrapped `hooks` form, `/bin/sh` command hooks, stdin JSON, exit status `2`,
and `hookSpecificOutput.permissionDecision` follow Claude Code conventions, so
the example scripts can be reused there. Keep their stdout empty unless they
are intentionally returning one JSON object.

The root-event `.clawq/hooks.json` form is the project-local, Codex-oriented
form parsed by Clawq. Codex itself uses its own hook configuration surface, so
install the command through that surface rather than assuming it reads
`.clawq/hooks.json` directly. The scripts do not depend on Clawq-only payload
keys; they inspect common `tool_input` fields and degrade safely when absent.
