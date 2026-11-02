# Coding-agent bridge investigation (B834)

Date: 2026-11-02. Status: investigation complete; operational gaps remain.

Existing external bridge CLIs support coding work in new T3 Code and Paseo
threads. Clawq has no built-in T3/Paseo runner, callback adapter, or
automatic bridge-skill discovery outside its own skill search directories.
Reuse the bridge implementations and deliberately expose reviewed instructions
to Clawq before considering another runner. Launching a thread and delivering
its result back to a particular Clawq session are separate operations.

## Authority and verification boundary

All sibling repositories and installed bridge files were inspected read-only.
No agent was launched, resumed, stopped, or archived, no wake was sent, and no
daemon or configuration was changed. Help, preset listings, project listings,
agent listing, target listing, and outbox health were the live checks. The launch
examples below were checked against source and help, **not executed**.

| Component | Authoritative source / observed installation |
| --- | --- |
| T3 bridge | `/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge`, repository HEAD `dc4c195412da0f7b7c7d476e03521876edbae59b`; clean bridge paths |
| T3 worktrees | `apmv-*` checkouts with `tools/t3-bridge` share Git common directory `/home/xertrov/src/agent-plugin-mgmt-and-vencord/.git`; their branches contain different, often older bridge implementations |
| T3 executable | `/home/xertrov/.local/bin/t3-bridge` points directly to that canonical checkout's `tools/t3-bridge/bin/t3-bridge` |
| Paseo source | `/home/xertrov/src/paseo-bridge`, HEAD `1a2d0a42c1abc57667fcbe9d26124186cad751e9`; clean checkout, separate Git common directory `/home/xertrov/src/paseo-bridge/.git` |
| Paseo executable | `/home/xertrov/.local/bin/paseo-bridge` points to release directory `/mnt/build-cache/paseo/bridge-releases/f68a26e175f3a820af8ef499e3a55fc13f9d291efdc36c783828295b301a5595`; this release identifier is not a Git commit |

The installed Paseo release **does not match the source checkout**: 13 of 19
compared Python/Node/entrypoint files differ or are missing. In particular,
`run_state.py`, `node/observe.mjs`, and `node/model.mjs` are absent in the installed
release. Its `ls` output can include archived agents; the current source excludes
archived agents unless `--all` is passed. Its fallback creates a workspace from
the original placement instead of reusing the actual workspace id, and its
`wait` passes a bare timeout number instead of the source's duration with `s`.
Do not assume the source README's newer recovery guarantees apply to the
installed command. See [P5] and [I1].

Observed at approximately 06:25 UTC on 2026-11-02:

- T3 `projects --json`: exit 0, 31 projects. Paseo `projects list`: exit 0,
  17 projects. Paseo `ls`: exit 0, 487 returned entries; this is not an active
  agent count because of the installed lifecycle-projection behavior.
- Both CLIs' help and preset listings succeeded. A preset listing describes
  configured selections; it does not prove provider capacity or model availability.
  A later read-only Paseo version/status check confirmed CLI and daemon versions
  both `0.13.2`; the bridge-release drift is separate from that daemon pin.
- T3 `target list` has no target named `clawq`. Its global fallback is `grokbot`,
  which is pending. Do not borrow another bot's caller name to obtain its route.
- T3 `outbox check` exited 1: `outbox check found failed or stale deliveries`.
  This establishes unhealthy shared delivery state, not which task failed.
  Queued payloads and credentials were not copied into the evidence log.
- No bridge-named skill entry was found in the inspected `.agents/skills`,
  `.codex-shared/skills`, `.claude-shared/skills`, `.llm-general/skills`, or
  `.clawq/skills` directories. T3's reviewed skill source exists at
  `tools/t3-bridge/SKILL.md`; installing a CLI does not install it into Clawq.

## Supported T3 new-thread flow

1. Run `t3-bridge projects --json` and select the actual project id or name.
   Supply `--cwd` or `--worktree` explicitly. For a registered root/subdirectory,
   the bridge selects the project's `root` strategy; for an existing linked
   worktree, it selects `existing_worktree` with that worktree's real root.
   These flags bind an existing checkout; they do not create a fresh worktree.
   A project-only launch uses its root. Without project/path flags the launch
   requests a T3 scratch thread; the shell's current directory does not register
   or select a project. [T1]
2. If registration is needed, the supported mutation is
   `t3-bridge --as CALLER projects add /absolute/repo --name NAME --json`.
   It requires a paired full-access token and an allowlisted caller. Ordinary
   project listing does not. Registration checks canonical path/Git identity
   and serializes by Git common directory within one bridge state directory.
   Creation is not atomic with local filesystem checks; lost acknowledgements
   require inspection, and the bridge never automatically deletes projects. [T2]
3. Launch with `--as CALLER`, `--title`, `--text`, an explicit preset, and
   `--json`. The CLI requires `--text`; unlike `send`, launch has no
   `--message-file` implementation. Its default preset is `sol-ultra`
   (`codex`, `ocx/gpt-6.1-sol`, `reasoningEffort=ultra`,
   `serviceTier=ultrafast`), runtime `auto`. Full-access runtime requires the
   explicit flag and an allowlisted caller. Capability lookup validates the
   chosen provider/model/options before creating the thread. [T3]
4. The wrapper calls `t3_thread_launch`, then `t3_thread_read` and
   `t3_thread_configuration`. Require exit 0, a nonempty `threadId`, and
   `verification.verified == true`. The readback checks saved provider, model,
   options, runtime, and thread identities. Persist the entire receipt, exit
   status, caller, originating Clawq session, checkout, and job correlation id.
   A nonzero exit can still include a created thread id: repair/inspect that
   thread rather than launching a duplicate. State-write failure likewise
   reports the created id. [T3]
5. The launch saves caller, wake target (the caller by default), tag, workspace,
   and verification in `$T3_BRIDGE_STATE_DIR/launches.json` (normally
   `~/.local/state/t3-bridge`). It appends the fleet build-storage rule and a
   completion reply footer to the brief. The new thread receives that brief
   and workspace context, not the parent's Clawq transcript. Include task,
   acceptance criteria, relevant paths, scope, and report requirements. [T3, T4]

The underlying API is MCP JSON-RPC over `POST <resolved T3 origin>/mcp`, using
a bearer token from the bridge configuration directory. The client initializes
protocol `2025-06-18`, retains the MCP session header, and sends `tools/call`.
The wrapper's creation arguments are:

```json
{
  "title": "Bounded coding task",
  "message": "Complete task brief and reply requirements",
  "projectId": "PROJECT_ID",
  "workspaceStrategy": { "type": "root", "branch": "BRANCH" },
  "runtimeMode": "auto",
  "modelSelection": {
    "instanceId": "codex",
    "model": "ocx/gpt-6.1-sol",
    "options": [
      { "id": "reasoningEffort", "value": "ultra" },
      { "id": "serviceTier", "value": "ultrafast" }
    ]
  }
}
```

Use `{ "type": "existing_worktree", "worktreePath": "ABSOLUTE_PATH",
"branch": "BRANCH" }` for an existing worktree; scratch requests use
`scratch: true` instead of a project id. These are wrapper-generated contracts,
not a proposed Clawq HTTP endpoint. `t3-bridge call t3_thread_launch --json JSON`
can call the raw tool, but bypasses the high-level launch's workspace resolution,
reply footer, launch registry, verification, and watcher. Raw launch also requires
an explicit model. Prefer the high-level command. The local `t3-bridge mcp`
stdio server advertises **only `grok_wake`**, not a launch tool. [T3, T5]

### T3 identities, completion, follow-up, and failure

`--as clawq` is a bot policy label, not a Clawq session id or a security boundary.
All bots share a Unix user. An allowlisted ordinary caller selects `token-full`;
other callers use `token`. A reporting T3 thread uses `t3:<full-thread-id>`
(for example, `t3:mcp:THREAD_ID`), always with the auto token. That identity
can wake only its recorded launcher target and cannot launch, send, settle,
register projects, or make generic calls. [T4]

The working thread must issue the completion report itself, preferably via
`grok_wake` with `target`, `message`, and `thread_id`. The outside-sandbox CLI
fallback is `t3-bridge --as t3:THREAD_ID wake --target CALLER --thread THREAD_ID
--message-file REPORT`. The full returned thread id, including any `mcp:` prefix,
must be preserved. Resolution otherwise tries `T3_THREAD_ID`, then a unique
active launch matching `T3CODE_WORKTREE_PATH`; it never infers attribution from
ordinary process cwd. Without context it can deliver a threadless wake. [T4]

Wake events have `event: "t3_thread"`, `event_id`, `thread_id`, `target`, caller
attribution, and a message. The shared durable outbox attempts delivery inline;
the existing two-minute timer retries pending delivery. Inspect the returned
delivery status: command success is not proof of delivery. Identical stable
wake fields deduplicate to one event id; the receiving adapter still needs
idempotent receipt handling and correct Clawq-session routing. A `queued` wake
has not reached Clawq. There is no old all-launch completion poller. [T4, T6]

Launch starts a detached stall watcher by default (60-second poll, 15-minute
silence window, 24-hour expiry); `--no-watch` disables it. Watcher spawn failure
does not fail the launch: it prints and records `watch_error`. Watches alert
about stalls/provider failures and do not auto-continue work. Dead/expired
watchers need explicit recovery; no service automatically resumes them. A
completion wake remains the agent's responsibility. [T6]

`send THREAD --text TEXT` defaults to `steer`; idle steering can retry with
`auto`, while busy unsteerable work is not silently queued. `queue` and `restart`
are explicit choices. Sends retain the thread's saved selection and use a stable
thread-and-undecorated-message request id; mode is not part of that id. `read`
and `wait --timeout SECONDS` inspect an existing thread. `settle THREAD` archives
a bridge-owned thread; unrelated threads require `--force`. Do not equate
archival with a generic cancellation operation. [T7]

For active-turn cancellation, the active **T3 Code 0.1.3** server advertises
`t3_thread_interrupt`, reachable through the bridge's generic `call` command.
Use an ordinary bot caller whose paired token has write authority for the
target; read-only clients are refused, and the server enforces the caller's
runtime/interaction-mode ceiling. Reserved `t3:THREAD_ID` callers cannot make
generic calls. The active tool describes top-level thread interruption and
directs delegated tasks to `task_cancel` instead. [T4, T5, T9]

Preserve the complete `threadId`. Optional `runId` pins the intended run;
without it, the newest interruptible run is selected. An explicit terminal run
returns its terminal status, an unknown/non-interruptible selected run is
rejected, and no active run without an explicit selection returns
`no_active_run`. `reason` is optional. Persist a stable `clientRequestId` for
the interruption request and retain the selected `runId` from the response. [T9]

Future mutating example, **not executed** (the wait/read commands are follow-up
inspection only; use the returned run id when `interrupt_requested` is returned):

```bash
t3-bridge --as CALLER call t3_thread_interrupt --json \
  '{"threadId":"mcp:RETURNED_THREAD_ID","clientRequestId":"STABLE_INTERRUPT_ID"}'
t3-bridge --as CALLER call t3_thread_wait --json \
  '{"threadId":"mcp:RETURNED_THREAD_ID","runId":"RETURNED_SELECTED_RUN_ID","timeoutMs":120000}'
t3-bridge --as CALLER read 'mcp:RETURNED_THREAD_ID' --json
```

`interrupt_requested` acknowledges dispatch, not terminal completion. Confirm
the **same thread and selected run** with read/run-specific wait and a terminal
status (`completed`, `failed`, `cancelled`, `interrupted`, or `rolled_back`).
`timedOut:true` is not confirmation; retain the pending state and follow any
completion-wake guidance in the wait result. Interruption is distinct from
archival, stopping a watcher, or disabling queued/future work. [T9]

`--fallback` is opt-in: an explicit launch-capacity rejection can step down Sol
effort, then try Cursor Auto. It does not replace a created thread, retry
ambiguous transport outcomes, or treat 429/reset/quota/timeouts as a safe launch
retry. MCP-session expiry resets the client session for a subsequent caller
attempt without replaying the mutation. The ordinary CLI client does not set a
request timeout; a bot needs a deadline and an uncertain-outcome state after
killing its client. Killing the client does not cancel the remote thread. [T8]

## Supported Paseo new-thread flow

Use the dedicated bot daemon, pinned to `@getpaseo/cli@0.13.2`, with
`PASEO_HOME=/mnt/build-cache/paseo/home` and endpoint `127.0.0.1:6768`.
The bridge refuses the desktop `~/.paseo` home (normally port 6767). Passwords
are loaded from the protected file/environment and passed through the child
environment, not command arguments. `doctor` checks the pin, daemon, provider
configuration, permissions, and launcher internals. Do not install, sync
providers, or start a daemon merely to perform this investigation. [P1]

The following recovery description applies to the **source checkout**, pending
installation alignment:

1. `paseo-bridge launch --preset sol --cwd CHECKOUT --title TITLE
   --message-file BRIEF` resolves a Git checkout through its common directory.
   It reuses/registers one project for the main root. By default it creates a
   **new** Paseo worktree off the caller checkout's current ref; uncommitted
   working-tree changes are not copied by that branch selection. `--no-worktree`
   instead runs in the supplied directory under the root project. [P2]
2. Python `PaseoClient.launch` calls the Node helper, which imports the pinned
   CLI's private `dist/utils/client.js`, connects to the home-selected daemon,
   calls `createWorkspace({source})`, then `createAgent({provider, model, cwd,
   workspaceId, title, modeId, thinkingOptionId, featureValues, initialPrompt})`.
   Worktree source is `{kind:"worktree", cwd:REPO_ROOT, action:"branch-off",
   projectId, baseBranch:CALLER_REF}`; direct-directory source is
   `{kind:"directory", path:CHECKOUT, projectId}`. Plain `paseo run` cannot
   supply the needed project id and feature values. This is a pinned client API,
   not a public bridge HTTP launch endpoint. [P2, P3]
3. The default `sol` preset selects `codex-ocx/ocx/gpt-6.1-sol`, ultra effort,
   `featureValues.service_tier="ultrafast"`, `modeId="full-access"`.
   `sol-xhigh`/`sol-high` are explicit alternatives. Claude UI uses subscription
   auth and bypass permissions; internal Cursor fallback sets auto-accept.
   These bridge-native identifiers do not change Clawq's `provider:model`
   configuration convention. [P4]
4. Save `agent_id`, `payload.workspaceId`, actual cwd, project id, and attempts
   from the JSON receipt. Top-level `preset`/`provider_model` describe the
   originally requested preset even when fallback changed the actual provider;
   use `payload` and `attempts` to establish what ran. A new agent receives
   `initialPrompt`, not a parent's conversation. No stable UI link is emitted
   by these bridge receipts; do not invent one. [P2, P3]

For launch-time capacity errors, both versions retry the same preset once,
then fall back to Cursor Auto; they do not automatically lower effort. The
new source additionally observes current provider lifecycle state in `read`
and `wait`, permits two same-agent continuations with 1s/2s backoff, then creates
a Cursor agent in the **same actual workspace** with original brief and handoff
instructions. Original-id `read`, `wait`, and `send` follow the saved replacement;
`wait` follows it through completion. `settle` addresses the exact selected id,
so each handoff agent needs deliberate cleanup. [P4, P5]

`read` and `wait` are consequently **not read-only probes** in either release:
they can send continuations or create fallback agents. The source journals
recovery reservations, uses process-shared locks and fsynced atomic writes,
and blocks replay after an uncertain recovery outcome. These protections do
not make independently repeated launch commands idempotent: each source launch
creates a new random launch journal, and the bridge does not preserve upstream
operation identities across helper invocations. Project, workspace, and agent
creation is not one bridge transaction. [P5, P8]

Pinned **Paseo 0.13.2 does provide upstream idempotency primitives**:
`createAgent` and `createWorkspace` accept `idempotencyKey`; the creation client
generates one when omitted. The server advertises `creationLifecycle`,
`agentRequestReceipts`, and `workspaceRequestReceipts`; receipt/lifecycle
behavior is capability-gated. Its creation service persists request
fingerprints, resource ids, progress/results, and `outcomeUnknown`. Sends accept
a caller-supplied `messageId`; server message receipts deduplicate by agent/id,
reject changed request fingerprints, and expose uncertain pending outcomes.
These primitives do not give this bridge caller-level exactly-once semantics.
[P8, P9]

The bridge supplies neither stable creation keys nor creation `onEvent`
observers, and its reduced receipt drops creation progress/uncertainty. Fresh
helper invocations obtain fresh keys, including its automatic same-preset
capacity retries and provider fallback selected by capacity/429/503 text
matching. Avoiding manual relaunches does not prevent those internal attempts.
The bounded bridge fix is to check advertised capabilities and persist
per-operation creation keys, send message ids, initial-prompt `clientMessageId`,
and exact request fingerprints before
dispatch; retain partial resource ids and creation/uncertainty receipts; then
reconcile uncertain outcomes before any retry or fallback. Retry the same
request with its recorded identity; a deliberately changed provider/request
needs a distinct operation identity, because reusing a key with a changed
fingerprint conflicts. Inspect after lost acknowledgement rather than blindly
relaunching. No such bridge plumbing is implemented in this lane. [P3, P5, P8, P9]

Source `wait --timeout 120` can return an outer `ok:true` with an inner JSON
`stdout` whose `status` is `timeout`; that is not completion. Parse both layers,
then verify the result/artifact. Its subprocess runner has a 60-second deadline
for non-wait operations; wait relies on its own timeout. A caller deadline must
exceed the requested wait plus observation/recovery overhead. [P5]

`send ID --text TEXT` follows a saved replacement in the new source. `stop ID`
targets the exact supplied agent, as does `settle ID`; select a replacement's
actual id when stopping or archiving that agent. Settle refuses running/active
work without `--force`, verifies archival, and archives an associated `wks_`
worktree workspace only when uniquely identified and unused; local checkouts
are not archived. Launch and settle also run cache maintenance that can remove
eligible bridge-owned build-cache entries. These are mutating workflows, not
status probes. [P5]

### Paseo completion is caller-managed

`launch --wake-target NAME` is accepted by help but ends in a `pass` in
`cmd_launch`, in both source and installed release. It neither stores the
target nor schedules/sends a completion wake. Paseo does not append the T3
reply footer. The reliable current contract is for the caller to persist the
agent/session association, wait/read through the bridge, and route the resulting
report itself. [P6]

`paseo-bridge wake --target NAME [--thread ID] MESSAGE` is a separate operation
that invokes `t3-bridge wake`. `--thread` therefore identifies a **T3 thread**
when that is the intended context; a Paseo agent UUID is not a T3 thread id.
Without a thread flag it uses a neutral runtime cwd to avoid repository-based
inference, but inherited T3 context variables can still affect T3's resolver.
For a Clawq-only caller, keep any Paseo agent id in the report/correlation record
and deliver to the mapped Clawq session; do not mislabel it as a T3 thread. [P6]

## Commands for a bot caller

Read-only discovery (the investigation executed these):

```bash
t3-bridge --help
t3-bridge preset list --json
t3-bridge projects --json
t3-bridge target list
t3-bridge outbox check
paseo-bridge --help
paseo-bridge launch --help
paseo-bridge preset list
paseo-bridge projects list
paseo-bridge ls
```

The following are **future mutating usage examples, not smoke checks**. Use an
owned checkout and a complete brief. The T3 example requires a real `clawq`
return route; the Paseo example assumes the installed release has first been
aligned with the source described above. Save stdout and nonzero exits in the
caller's durable job record; never run another launch just because verification
or observation failed.

```bash
bridge_checkout=/absolute/path/to/owned/worktree
bridge_caller=clawq

# Existing registered project/worktree; the path selects its owning project.
t3-bridge --as "$bridge_caller" launch --cwd "$bridge_checkout" \
  --preset sol-ultra --title 'Bounded task' \
  --text 'Job JOB_ID: review README.md for broken local file links. Do not edit files. Report findings, head SHA, inspected paths, verification, and remaining concerns.' \
  --json

# Set this to the complete threadId from the saved receipt.
t3_thread_id=mcp:RETURNED_THREAD_ID
t3-bridge --as "$bridge_caller" read "$t3_thread_id" --json
t3-bridge --as "$bridge_caller" wait "$t3_thread_id" --timeout 120

# Paseo creates a fresh worktree by default; brief file already exists.
paseo-bridge launch --preset sol --cwd "$bridge_checkout" \
  --title 'Bounded task' --message-file /absolute/path/to/brief.md

# Set this to agent_id from the saved receipt; these may perform recovery.
paseo_agent_id=RETURNED_AGENT_ID
paseo-bridge wait "$paseo_agent_id" --timeout 120
paseo-bridge --json read "$paseo_agent_id" --after 0
```

For T3 the worker's completion tool call is, using its full returned id:

```json
{
  "target": "clawq",
  "thread_id": "mcp:RETURNED_THREAD_ID",
  "message": "JOB_ID: result; head SHA; changed paths; checks; remaining concerns"
}
```

This is the argument object for `grok_wake`, not an HTTP body for Clawq. The
receiving adapter must translate/authenticate it and look up the originating
session. Existing Clawq injection accepts `session_key` and `message` at
`POST /session/inject`, with its normal auth/pairing rules; it does not consume
the T3 wake envelope directly. A trusted caller can alternatively use the
documented `clawq session inject SESSION MESSAGE...` command after collecting
a report. Neither path establishes job/event deduplication by itself. [C3]

## Gaps, recommendations, and bounded follow-up

| Finding | Consequence / next bounded action |
| --- | --- |
| Installed Paseo release trails source recovery/lifecycle fixes | Align the installed bridge with a reviewed source release before relying on workspace-preserving fallback, timeout handling, or archived-agent filtering. This investigation does not install it. |
| T3 has no `clawq` target; shared outbox check fails | Establish an owned callback adapter and diagnose affected delivery state before claiming automatic return-to-Clawq. Do not flush/retry unrelated events as a test. |
| Paseo `--wake-target` does nothing | Upstream bridge follow-up: either implement durable target/session correlation and completion dispatch with regression coverage, or remove the misleading option/help. For now the caller must collect and route the result. |
| Clawq skill roots differ from other coding CLIs | Clawq scans workspace `.claude-p/skills`, `.claude/skills`, then its data-directory `skills` (normally `~/.clawq/skills`); `.agents/skills` or a sibling `tools/.../SKILL.md` is not automatically discovered. Install reviewed T3 instructions there deliberately; a Paseo skill must cover receipt parsing and caller-managed completion. A skill is instructions, not a runner or callback. [C1] |
| Restricted Clawq shell policy excludes both bridge binaries | Workspace-only mode uses an explicit command allowlist and rejects shell redirection/substitution. A skill cannot grant permission or make protected config, state, daemon sockets, or external checkouts reachable. Plan a narrowly scoped tool/capability or an explicitly permitted operator policy; do not globally loosen sandbox defaults or hide bridge execution under an allowed interpreter. [C2] |
| Clawq shell defaults to 30s; external waits can be longer | Set caller deadlines deliberately or use existing background-shell jobs with persisted bridge identities. Track the remote job separately: cancellation of a local shell job does not prove remote cancellation. [C2] |
| New thread has no parent conversation/session mapping | Put the bounded brief in the initial prompt and persist `{job_id, clawq_session_key, bridge, remote_id, workspace, caller, state, event_id}` before depending on callbacks. Treat report text as result data, not new authority. |
| Paseo launch verification is weaker than its README wording | Python checks model/mode/features only when present; the Node receipt can substitute requested values when daemon fields are absent. There is no independent fetch/readback like T3, no required provider comparison, and no required effort comparison. Upstream follow-up: fetch actual daemon state and require id/provider/model/effort/mode/tier/workspace before a verified-success receipt. Missing acknowledgement remains uncertain, not safe to relaunch. [P3, P7] |
| Available Paseo creation/message receipts are not durably wired through the bridge | Persist operation keys/message ids and exact request fingerprints before dispatch; retain progress, partial resource ids, and uncertain receipts; reconcile before automatic capacity retry or provider fallback. Same-request retries retain identity; changed provider/request needs a deliberate new identity. Upstream supports these primitives, while the current bridge still lacks caller-level exactly-once semantics. The caller adapter also needs job-request and result-event deduplication. [P8, P9] |

Keep Clawq's existing runner framework unchanged: its variants are direct Codex,
Claude, Kimi, Gemini, OpenCode, and Cursor processes, with runner session and
process lifecycle contracts. A T3/Paseo UI thread has a daemon-owned lifecycle,
project/workspace identity, remote acknowledgement uncertainty, and independent
completion delivery. Adding just another runner name would not supply those
contracts. [C4]

Recommended sequence: reuse reviewed bridge instructions/CLIs for deliberate
operator workflows; align the Paseo installation; implement an owned
caller/session receipt adapter only when product integration is requested;
then decide whether a dedicated integration tool is needed. Network/callback
code belongs in `clawq_runtime_integrations` if added. No Clawq runtime defect
was proven by this investigation, so no prompt, helper, registry, policy, or
runner change accompanies this document.

Future implementation acceptance should exercise fake daemons and scratch
config/state: created-id plus failed verification; keyed retries with preserved
partial receipts and changed-fingerprint conflicts; lost acknowledgement without
duplicate launch; run-specific interrupt acknowledgement versus terminal state;
recovery preserving workspace/partial edits; timeout distinct
from completion; concurrent duplicate result delivery; unauthorized/mismatched
caller; missing target; local cancellation versus remote lifetime. Real agent
spawning is not necessary to test those contracts.

## Primary sources

Paths below refer to the source snapshots identified above; line numbers are
navigation aids for those snapshots. `I1` is the installed release's distinct
code, not evidence that it matches source.

T3 API sources below were inspected read-only at reference checkout HEAD
`fddc5fa583d66a00698e0dc33ee89d2c7e1277c6`; fresh MCP discovery separately
confirmed the active 0.1.3 interrupt/wait schemas without invoking either tool.

- [T1]: [workspace facts](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/workspace-facts.mjs:14), [workspace strategy](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/workspace-resolution.mjs:1).
- [T2]: [registration and recovery contract](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/SKILL.md:56), [project command](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/cli/commands/projects.mjs:1).
- [T3]: [CLI launch flags](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/cli/commands/launch.mjs:4), [launch/receipt/state/watcher](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/launch.mjs:24), [verification](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/launch-verification.mjs:6), [capability check](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/capabilities.mjs:58).
- [T4]: [caller policy](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/caller.mjs:1), [launch prompt](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/launch-prompt.mjs:1), [thread context](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/thread-context.mjs:1), [wake dispatch](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/wake.mjs:50).
- [T5]: [raw call](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/cli/commands/call.mjs:6), [MCP transport](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/mcp-client.mjs:111), [wake-only stdio tool list](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/mcp-server.mjs:44).
- [T6]: [routing/outbox/watch documentation](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/README.md:395), [wake statuses](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/wake.mjs:94).
- [T7]: [thread commands](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/cli/commands/thread.mjs:15), [send/read/wait](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/thread.mjs:1), [settle](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/settle.mjs:1).
- [T8]: [fallback classification](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/t3/presets.mjs:41), [MCP timeout/session handling](/home/xertrov/src/agent-plugin-mgmt-and-vencord/tools/t3-bridge/src/mcp-client.mjs:101).
- [T9]: [interrupt/wait tool definitions](/home/xertrov/src/refs/t3code/apps/server/src/mcp/toolkits/orchestrator/tools.ts:248), [write-authority handler](/home/xertrov/src/refs/t3code/apps/server/src/mcp/toolkits/orchestrator/handlers.ts:143), [server caller/mode checks and acknowledgement](/home/xertrov/src/refs/t3code/apps/server/src/mcp/OrchestratorMcpService.ts:2576), [run selection](/home/xertrov/src/refs/t3code/apps/server/src/orchestration-v2/ThreadManagementService.ts:962), [input/result schemas](/home/xertrov/src/refs/t3code/packages/contracts/src/orchestratorMcp.ts:443).
- [P1]: [installation/pin/auth](/home/xertrov/src/paseo-bridge/README.md:1), [path/home guards](/home/xertrov/src/paseo-bridge/paseo_bridge/paths.py:12).
- [P2]: [launch receipt](/home/xertrov/src/paseo-bridge/paseo_bridge/cli.py:164), [project/workspace selection](/home/xertrov/src/paseo-bridge/paseo_bridge/client.py:779).
- [P3]: [pinned client API and receipt](/home/xertrov/src/paseo-bridge/paseo_bridge/node/launch.mjs:63).
- [P4]: [presets/launch fallback](/home/xertrov/src/paseo-bridge/paseo_bridge/presets.py:1).
- [P5]: [launch journal](/home/xertrov/src/paseo-bridge/paseo_bridge/client.py:543), [read/send/wait](/home/xertrov/src/paseo-bridge/paseo_bridge/client.py:208), [recovery and same-workspace handoff](/home/xertrov/src/paseo-bridge/paseo_bridge/client.py:666), [atomic storage](/home/xertrov/src/paseo-bridge/paseo_bridge/run_state.py:13), [recovery contract](/home/xertrov/src/paseo-bridge/docs/recovery.md:1).
- [P6]: [no-op wake target](/home/xertrov/src/paseo-bridge/paseo_bridge/cli.py:194), [wake forwarding](/home/xertrov/src/paseo-bridge/paseo_bridge/cli.py:231).
- [P7]: [conditional launch checks](/home/xertrov/src/paseo-bridge/paseo_bridge/client.py:872), [actual-state observation helper](/home/xertrov/src/paseo-bridge/paseo_bridge/node/observe.mjs:13).
- [P8]: [pinned creation key/options contracts](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/client/dist/daemon-client.d.ts:181), [client key generation/fingerprints/observers](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/client/dist/creation/index.js:56), [server durable creation receipts/uncertainty](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/server/dist/server/server/creation/index.js:25), [advertised capabilities](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/server/dist/server/server/websocket-server.js:1378).
- [P9]: [caller-supplied send message id](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/client/dist/daemon-client.js:2353), [server send receipt dispatch](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/server/dist/server/server/session.js:6341), [message receipt fingerprints/deduplication/uncertainty](/mnt/build-cache/paseo/releases/0.13.2/node_modules/@getpaseo/server/dist/server/server/message-receipts/index.js:17).
- [I1]: [installed wait](/mnt/build-cache/paseo/bridge-releases/f68a26e175f3a820af8ef499e3a55fc13f9d291efdc36c783828295b301a5595/paseo_bridge/client.py:234), [installed recovery/fallback](/mnt/build-cache/paseo/bridge-releases/f68a26e175f3a820af8ef499e3a55fc13f9d291efdc36c783828295b301a5595/paseo_bridge/client.py:666), [installed workspace creation](/mnt/build-cache/paseo/bridge-releases/f68a26e175f3a820af8ef499e3a55fc13f9d291efdc36c783828295b301a5595/paseo_bridge/node/launch.mjs:81).
- [C1]: [Clawq skill search roots](../../src/skills.ml), lines 479-486, and instruction-only skill tool, lines 659-675.
- [C2]: [Clawq shell allowlist/deadlines/policy](../../src/tools_builtin_util.ml), lines 74-113 and 426-550; [background-shell tools](../../src/tools_bg_shell.ml).
- [C3]: [authenticated session injection](../../src/http_server.ml), lines 163-200; [injection CLI](../cli-reference.md), `clawq session inject`.
- [C4]: [runner types and commands](../../src/runner_framework.ml), lines 1-62.

## Verification and integration handoff

The lane report and command transcript are under
`.superpowers/sdd/b816-b835/task-B834-report.md` and
`.superpowers/sdd/b816-b835/b834-readonly-verification.jsonl` in the assigned
worktree. Runtime RED/GREEN is not applicable to this documentation-only
investigation. The coordinator supplied the canonical x-p35 quick-suite baseline:
**7,746 tests, GREEN, 62.056 seconds**, in the memory lane's
`.superpowers/sdd/b816-b835/baseline-tests.log`. No local suite was run.
The scoped review correction is recorded in `task-B834-fix-report.md`; its
read-only `initialize`/`tools/list` evidence is in
`b834-fix-mcp-discovery.json` beside that report. No interruption was executed.

Coordinator-owned integration docs should summarize the external workflow and
its readiness gaps in the background-task guide, CLI references, room-agent
architecture, and public llms reference. They must continue to distinguish
external bridge delegation from built-in runners. No shared documentation is
changed in this lane.
