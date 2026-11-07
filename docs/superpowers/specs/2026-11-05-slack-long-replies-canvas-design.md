# I065: long Slack replies in canvases

Design date: 2026-11-05. Binding input: `.superpowers/sdd/2026-11-05-slack-canvas-replies/scope-ruling.md` (R1–R8), overriding `requirements.md` in that directory and the previous design wherever they conflict. This specifies future implementation, not deployment.

## Intent and boundaries

Above 2500 Unicode code points, put Clawq's complete final Slack answer in a read-only channel canvas and post its opening paragraph plus a link. DM/MPDM overflow becomes a `.md` file. Channel delivery falls back canvas → exact Markdown file → existing full-text send. An explicit research tool creates channel canvases.

Final answers only: progress, thinking, notices, receipts, cards and standing-order relays retain ordinary senders. No HTML hosting, public/org-wide sharing, channel canvas tabs, general upload tool or second-model summarisation. Preserve current allowlists, requester authorization and `private_channel_policy`.

## Controller rulings

- **R1:** In-process idempotency only: one delivery call attempts create at most once; internal retries of later steps reuse the known canvas ID. A crash, restart or queue replay may leave an extra private canvas or duplicate summary/file post. This accepted limitation has no cross-crash/replay idempotency tests.
- **R2:** Hook live `Slack.handle_message` finals, native-thread queue/drain/replay finals and `Connector_dispatch` follow-up finals. Extend other producers only at small, clean final call sites; list remaining gaps below.
- **R3:** Automatic DM/MPDM overflow is file-only; the explicit tool returns the prescribed DM guidance. No recipient enumeration or user grants.
- **R4:** Create → `files.info` → share summary/link → channel read grant. Failure triggers best-effort deletion of the new canvas, removal of its carrier link (or carrier deletion), then file fallback. The brief share-before-grant interval is accepted.
- **R5:** Minimal Markdown preparation outside code fences only; no table-cell, heading-depth or HTML validator. API rejection selects the exact file.
- **R6:** Implementation waits for P3's merge and this branch's rebase onto master. First extract P3's upload-v2 primitives into shared `Slack_file_upload`; relay and I065 both use them.
- **R7:** Keep code-point thresholds, the 600-character opening paragraph, B845, validated/live config, authoritative permalink, standalone canvas, fallbacks, tool/prompt/templates, runtime split, transport extraction, docs and redacted logs.
- **R8:** Fake-HTTP tests cover the requirements except cross-crash/replay idempotency, plus one-create-on-later-failure and DM file delivery.

## Final routing and current code

`Slack_thread_runs.open_stream` sends `Working…`; maintenance rotates status and `finish` stops it. Status-only stream messages are deleted on rotation and completion; completed control cards are cleared and deleted, while stopped tasks retain their stop receipt. Cleanup uses current authorization and is best effort: denied or failed cleanup may leave temporary UI visible without suppressing the final answer. This checkout has no answer `chat.appendStream` calls. `Status_update.make_handler` ignores answer deltas. Keep answers buffered in every mode, including `off`: finish status/progress, then send one final presentation. Provider streaming is not Slack answer streaming.

| Producer | Final-only hook |
| --- | --- |
| `Slack.handle_message` | Replace final `reply ~text:response` in `before_drain` and result branches. Retain `response_sent` convergence, original channel and optional parent (`None` for a root). Thinking/error receipts stay ordinary. |
| Native queue drain | `Session_turn.drain_queued_messages_loop` has one final `notify response` after `execute_queued_request`. Add a separate optional core queued-final callback there, defaulting to the existing notifier. Slack derives target/requester from the actual queued item and verified native route, never the previous live closure. |
| Native replay | Change only `Daemon_inbound_replay.native_replay_turn`'s final `before_drain` send to the same final sender. Preserve queue execution, ownership and existing failure handling. |
| `Connector_dispatch` follow-up | Add optional `send_final`, default `send_reply`, to `dispatch_followup` and its environment. Both idle final branches use it; queue/append acknowledgements and progress use `send_reply`. Busy follow-ups use the queue-final hook for native routes and legacy Slack follow-up items. |
| Agent commands | Add optional `send_final`, default `send_reply`, to `Session_agents.agent_invoke_turn`, `delegate_turn`, `fork_and_run`. Only completed `Agent.turn` answers use it; Slack supplies it. Document notices, validation/errors and debug output stay ordinary. |
| Live autonomous continuation | The two `Slack.handle_message` `process_autonomous_turn_result` callbacks are clean final-only sites; supply the final sender with the original target. |
| Known gaps | Direct background completions in `Daemon_util.inject_background_task_completion`, daemon-resume/autonomous finals through generic resumed-session notification, and non-native queued finals other than follow-ups remain ordinary sends. They lack a clean existing final-target seam; do not expand shared notification machinery. Native queued outcomes of these producers are covered by the native queue-final hook. |
| Relays, progress, notices, receipts, cards | Bypass overflow. Never wrap `Slack_api.send_message` or `Slack_thread_execution.notify` globally. |

The optional manager callback takes `key`, the existing queued item and final text; it adds no fields to that item. Derive native root/channel from `Slack_thread_route` and current authorization. Legacy Slack follow-ups use their existing `channel_id`/requester and historical root delivery (`thread_ts=None`); other non-native items keep the old notifier. Never borrow another live turn's parent. Honor suppressed/blank replies and suppressed channel output; mark sent only after send success, preserving deferred-response semantics.

`Slack_huddle_context.clear` precedes live final sending. Capture the server-owned destination in the final closure before clearing it; reauthorize using current config before outward operations. Tool authorization separately uses `Slack_huddle_context.resolve` and snapshot checks. Presentation metadata never grants authority.

## Task 4 implementation and remaining gaps

Automatic final delivery now uses a captured destination and a checked text
adapter through `Slack_final_reply.send`. Overflow presentation uses checked
conversation metadata, with policy frozen before metadata I/O. Native queue drains and replay derive
fresh adapters from their own queued requester or synthetic owner and original
thread route. Legacy queued follow-ups retain their historical top-level target.
The core queue hook defaults to the existing notifier; other connectors retain
ordinary sends. Agent invoke/delegate/fork completed answers and both live Slack
autonomous callbacks use the final sender; their document notices, errors and
debug messages remain ordinary sends. Required answers are marked sent only
after successful final dispatch; a failed final leaves its existing pending
state intact even if an error notice is posted.

Direct background completions, daemon-resume/autonomous finals through generic
resumed-session notification, and non-native queued finals other than follow-ups
still use ordinary sends. Native queued outcomes of these producers are covered.
These are deliberate routing gaps; this task adds no shared recovery or
notification mechanism. Delivery ownership remains call-local: a crash, restart
or replay can leave an extra canvas or duplicate carrier/file post (R1).

## Slack API contracts

These public references define the selected flow; design and tests never call live Slack.

| Contract | Source and design consequence |
| --- | --- |
| `canvases.create` accepts optional `title`, `document_content = {type:"markdown", markdown:…}` and `channel_id`; returns `canvas_id`. Scope `canvases:write`. | [create](https://docs.slack.dev/reference/methods/canvases.create/). Create standalone, omitting `channel_id`. |
| With `channel_id`, creation adds a canvas tab with write permissions. Free teams require it and may return `free_teams_cannot_create_standalone_canvases` or `free_team_canvas_tab_already_exists`. | [create](https://docs.slack.dev/reference/methods/canvases.create/). Never retry with `channel_id`; use file fallback. |
| Content limit is “1 MiB (1,048,576 characters)”; supported Markdown includes tables (300 cells), code, h1–h3, lists, links and mentions. Block Kit is unsupported. | [Canvas surface](https://docs.slack.dev/surfaces/canvases/). Keep conservative 1,048,576 byte/code-point ceiling after preparation. Let Slack reject unsupported structures; no structural validator. |
| Canvas mentions use `![](@U…)` and `![](#C…)`. | [Canvas surface](https://docs.slack.dev/surfaces/canvases/). Adapt message tokens outside fences only. |
| `canvases.access.set` requires canvas ID, access level and either channel IDs or user IDs; scope `canvases:write`. Share first, then change access at that level. | [access.set](https://docs.slack.dev/reference/methods/canvases.access.set/). Use `read`, `channel_ids:[originating_channel]` only, after posting. |
| Access setters reject DM/MPDM conversation IDs; DM sharing would require user grants. | [DM/MPDM subsection](https://docs.slack.dev/reference/methods/canvases.access.set/#sharing-canvases-via-dm-or-mpdm). R3 selects conversation-scoped files. |
| `files.info(file=canvas_id)` requires `files:read`; file objects expose `permalink` separately from `permalink_public`. | [files.info](https://docs.slack.dev/reference/methods/files.info/), [file object](https://docs.slack.dev/reference/objects/file-object/). Use returned HTTPS permalink, never a public or fabricated URL. |
| `chat.postMessage` takes channel and optional parent `thread_ts`; scope `chat:write`. Canvas preview layout is not guaranteed. | [postMessage](https://docs.slack.dev/reference/methods/chat.postMessage/), [unfurling](https://docs.slack.dev/messaging/unfurling-links-in-messages/). Label link; disable unfurls and broadcast. Existing full-text fallback retains Slack's documented 40,000-character limitation. |
| `files.getUploadURLExternal` takes filename and byte length, returns upload URL/file ID; bytes upload separately. Scope `files:write`. | [getUploadURLExternal](https://docs.slack.dev/reference/methods/files.getUploadURLExternal/). Reuse P3's checked GET/query adapter and raw POST; no bot authorization on upload host. |
| `files.completeUploadExternal` takes files, channel ID, optional `thread_ts`, `initial_comment`; complete once. Without channel, file stays private. | [completeUploadExternal](https://docs.slack.dev/reference/methods/files.completeUploadExternal/). Exactly one originating conversation, original parent, summary as comment. |
| `conversations.info` distinguishes IM/MPIM/private metadata; `conversations.members` is paginated. Read scopes include `channels:read`, `groups:read`, `im:read`, `mpim:read`. | [info](https://docs.slack.dev/reference/methods/conversations.info/), [members](https://docs.slack.dev/reference/methods/conversations.members/). Use typed metadata, not ID prefixes. Preserve existing membership authorization; add no recipient enumeration for grants. |
| `auth.test` returns team ID/workspace URL with no added scope. | [auth.test](https://docs.slack.dev/reference/methods/auth.test/). Preserve existing native installation checks; never fabricate a permalink from a workspace URL. |
| `files.delete` deletes an owned file; scope `files:write`. | [files.delete](https://docs.slack.dev/reference/methods/files.delete/). Only checked successful deletion of this call's allocated file permits fallback after unresolved completion. |
| `canvases.delete` permanently deletes a canvas; scope `canvases:write`. | [delete](https://docs.slack.dev/reference/methods/canvases.delete/). Cleanup only this call's newly created canvas. |
| Create errors include `canvas_creation_failed`, `canvas_disabled_user_team`, `missing_scope`, `restricted_action`, `ratelimited`, `internal_error`, `fatal_error`; partial success is possible. No client idempotency argument is documented. | [create errors](https://docs.slack.dev/reference/methods/canvases.create/). Never retry create within the call, even on unknown outcome; reuse known ID for later steps or fall back. |

The controller reports `files:read` and `canvases:read/write` are granted, checked 2026-11-05; this run does not inspect credentials. Selected canvas flow needs `canvases:write`/`files:read`, uploads `files:write`, posts `chat:write`, plus existing conversation scopes. Setup explains reinstall for added scopes. Coordinator acceptance after deployment verifies channel/private-channel read-only access and the brief no-access interval; DM/MPDM checks verify files only.

## Policy and content

Overflow is `code_points(original_body) > threshold`; equality stays in chat. Count validated UTF-8 scalars, not bytes/graphemes. Blank/suppressed answers create no artifact or empty post (B845). Invalid UTF-8 uses existing full-text handling without replacement characters.

For summary extraction only, normalize CRLF/CR to LF and trim. Take the first non-empty run of lines ending at a blank/spaces-only line outside a backtick/tilde fence (at least three matching characters, closed by the same character with sufficient length). Leading headings, lists or code belong to this opening block. Join line breaks with spaces. Cap at 600 scalars including `…`: take 599, right-trim, append ellipsis. Without a paragraph break, cap the whole-body prefix. Empty extraction uses `Full answer`. Escape Slack `<`, `>` and `&` after measuring; disable mention expansion. Append a trusted labelled `Read full answer` link separately.

Canvas Markdown is unchanged except outside fenced code: `<@U…>` → `![](@U…)`, `<#C…|label>` → `![](#C…)`, `<url|label>` → `[label](url)`; render `<!here>`, `<!channel>`, `<!everyone>` inert (e.g. `&lt;!here&gt;`). No HTML, heading-depth or table-cell validation. Slack rejection selects the file. File content preserves the entire original bytes, including its opening paragraph. Plain chat finals and full-text fallback adapt Markdown emphasis, headings and links to Slack mrkdwn once at the final sender, preserving fenced and inline code. Overflow selection and canvas preparation receive the original Markdown. No image fetching, extra history or hidden metadata. Automatic title is `Clawq answer` plus source timestamp when available; explicit title is trimmed, valid UTF-8, non-empty and at most 120 scalars.

## Delivery and failures

1. Authorize the original destination with current config, allowlist, requester and private-room policy. Obtain typed conversation metadata. Security/destination denial stops all writes, including fallback. If only canvas eligibility is unavailable but the original target remains independently authorized, select file/text there.
2. Freeze current `long_reply` policy at delivery entry. Short/`off` answers use text. `file` or any DM/MPDM overflow uses file directly. Eligible channel overflow in `canvas` mode attempts standalone create once.
3. Hold returned ID locally, fetch permalink, post escaped summary/link at the original parent, then grant that channel read access. Success requires the grant.
4. Any canvas failure: best-effort delete the new canvas, edit the known carrier to drop its link (or delete it), then attach exact `.md` with summary comment. Cleanup failure never suppresses fallback; failed edit and delete can leave a stale carrier. Never target unrelated artifacts/messages.
5. A definite file failure permits exact full-text send through the checked text adapter below, without raw API errors or blank text. An ambiguous completion must first reconcile using the allocated file ID, as specified below. If text sending fails, propagate existing transport failure and do not mark sent. Existing Slack size/truncation limits remain the last fallback's limitation.

Bound internal retries of safe later stages (permalink/read grant) and retain the same ID. Timeout/malformed/unknown create results never authorize another create in that call. Do not repeat completion for a ticket with ambiguous completion result. The upload adapter returns `Shared`, `Definitely_not_shared` or `Unresolved file_id`, separately from authorization/cancellation failures. A successful HTTP/Slack completion alone is insufficient without validated evidence for that file and the original conversation/parent; timeout, malformed response, 5xx or absent share metadata is not proof of failure. Reconcile with at most three checked `files.info` reads of the allocated file ID within a ten-second total deadline (injectable clock/wait for tests), inspecting public/private share records for the exact channel and parent (root versus thread); never accept another target's share. Confirmed sharing returns `Shared`, hence `File_sent`, with no text post. Reuse P3's evidence interpretation where applicable, without its durable orchestration. Only a local failure before completion dispatch, an explicit pre-share rejection such as `invalid_arguments`, or checked successful deletion of this call's owned file establishes `Definitely_not_shared`. Empty/eventually consistent share records, lookup errors and `file_not_found` alone do not. After bounded reconciliation remains unresolved, attempt checked `files.delete` on that owned ID if currently authorized; confirmed deletion permits text fallback. If deletion is denied, fails or is ambiguous, return a safe `Completion_unknown` failure, make no second full presentation and do not mark sent. Preserve existing failure retention; this may leave a shared file with unconfirmed delivery. Add no automatic completion retry, new recovery loop or persisted state. Propagate cancellation; refresh authorization, credentials and egress per request. Delivery has no disk state or model/recovery changes. Explicit tool invocations are independent calls; no persisted tool/final deduplication contract.

All ordinary final text paths (short, `off`, invalid-UTF-8 handling and full-text fallback) use one explicit `Slack_final_text.send` adapter, carried by `Slack_reply_delivery.deps.post_text`. Pass it into `make_deps` and `Slack_final_reply.send`; the live final closure captures the extracted transport's adapter before huddle context clears. Queue/replay/follow-up closures construct an adapter for their own verified target, never reuse the preceding turn's closure. The production adapter validates both 2xx HTTP status and parsed Slack `ok:true`; non-2xx, `ok:false`, malformed JSON, transport errors and cancellation cannot become `Text_sent` or set `response_sent`. Use checked native calls and a checked legacy `chat.postMessage` request, preserving original parent and current authorization/credentials/egress. Bind the adapter to the verified destination/route and reject mismatching channel/parent arguments. Do not delegate production final success to `Slack_api.send_message`/`send_message_reply`, which discard status and `ok`.

Preserve the existing optional injected legacy unthreaded sender: distinguish an explicitly supplied `send_message_fn` from the production default at `Slack.handle_message` entry. Wrap that trusted injection into the same result-bearing adapter (resolved unit means success; exception means safe failure; cancellation propagates). Explicit legacy parents retain their threaded checked API and native routes retain native checks. This preserves the injection's supported scope. Ordinary notice/progress helpers retain their current semantics; no global sender rewrite.

Log only stage, safe error class/Slack code, artifact ID and fallback outcome. Never log tokens, signed upload URLs, answer text, summaries, titles, raw bodies or exception strings. Upload capabilities bypass HAR capture and are redacted in audit output. This includes the real `Agent_2_tools.execute_tools` boundary: redact the entire `slack_canvas_create` argument string before INFO logging and `Audit.ToolInvocation.args_preview`, before parsing or permission checks. Redact result/error previews and failure reasons too, since parse/validation errors can echo raw arguments. Malformed/empty/denied calls receive the same protection. For raw-call diagnostics, suppress the entire provider raw batch whenever any call in it is `slack_canvas_create`, including a failure in a sibling call; if the raw batch is malformed/unclassifiable, suppress it conservatively rather than dump it. Use fixed safe markers/classes, not content-derived previews. Keep original arguments for execution, tool events and model history, and original results for model history; apply redaction only to logging/audit projections. Both buffered and streaming executor entry points use this projection. Add no invocation persistence, recovery or tool-execution semantic changes.

## Shared upload extraction (R6)

Read-only inspection of P3 (`feat/huddle-standing-orders`, `/home/xertrov/src/clawq/.worktrees/huddle-orders`) found allocation/completion in `Slack_standing_order_dispatch.deliver`, checked API calls in `Slack_standing_order_slack`, and raw upload/pinning/preflight in `Slack_standing_order_upload`. Extract primitives, preserving P3 orchestration.

Exact shared integration-only interface:

```ocaml
(* Slack_file_upload: no Session, Agent or standing-order dependency *)
type request = { uri : string; headers : (string * string) list; body : string }
type response = { status : int; headers : (string * string) list; body : string }
type raw_http = request -> response Lwt.t
type 'e call = string -> (string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, 'e) result Lwt.t
type 'e error = Remote of 'e | Invalid_response | Invalid_upload_url
              | Http_status of int | Transport
type ticket = { file_id : string; upload_url : string }
val default_http : raw_http
val pinned : string -> bool
val allocate : call:'e call -> filename:string -> length:int ->
  (ticket, 'e error) result Lwt.t
val post : ?http:raw_http -> limit:(unit -> unit Lwt.t) ->
  authorize:(uri:string -> (unit, 'e) result) ->
  before_send:(unit -> (unit, 'e) result) -> uri:string -> body:string ->
  unit -> (unit, 'e error) result Lwt.t
val complete : call:'e call -> channel_id:string -> thread_ts:string option ->
  initial_comment:string -> file_ids:string list ->
  (Yojson.Safe.t, 'e error) result Lwt.t
```

`allocate` validates nonnegative byte length, file ID and pinned URL. `complete` rejects empty/duplicate IDs, emits channel/optional thread/comment and calls once. Both consume the caller's checked API closure, preserving limiter/config checks and synchronous `before_send` immediately before HTTP. `post` pins URL, awaits limiter, calls `authorize` on the actual sensitive URI/current config, then `before_send` synchronously before one raw request. Preserve P3's 2xx success semantics, typed errors, cancellation, no redirects, no bot auth, no capture, strict audit. Callbacks are trusted adapters, not model inputs.

P3 call sites that switch:

- `Slack_standing_order_upload`: shared aliases for raw request/response/HTTP, `default_http`, `pinned`; existing `post` remains a compatibility wrapper around shared `post`. Its authorization calls existing `SO.preflight ~scope:Standing_order ~sensitive_uri:true`; retain snapshot/allowlist checks. Wrapper maps shared failures back to `SO.error`.
- `Slack_standing_order_dispatch.deliver`: per-file `getUploadURLExternal` becomes shared `allocate`; retain saved ID/duplicate checks and existing wrapper call for byte POST. Batch `completeUploadExternal` becomes shared `complete` using the existing checked completion closure, same IDs/comment and `thread_ts:None`. Preserve synchronous reservation, completion evidence, reconciliation and retries.
- The compatibility wrapper maps `Remote e` back to `e`, invalid response/transport to `SO.Transport`, invalid URL to `SO.Egress_denied "upload"`, HTTP status to `SO.Http_status`. Dispatch allocation handles shared local validation errors separately as the existing terminal `upload_allocation_invalid`; do not turn malformed tickets into transient `SO.Transport` retries. Remote allocation errors retain existing `precomplete_error` handling. Do not alter `Slack_standing_order_slack.make`, its closed method list, preflight or top-level-only completion rule. I065 supplies separate checked callbacks allowing its original thread/DM target; never broaden relay policy.

## Config and module graph

`channels.slack.long_reply = {mode: canvas|file|off, threshold: 1..40000}`, default `canvas/2500`. Missing fields default independently. Invalid loaded fields/unknown mode warn safely and default that field. `config set` validates leaves, object and nested parent updates; rejects unknown keys, null/float/bool/range errors with key and example. Include effective values in serialization/show/search/tree.

Read settings at final entry; render guidance from current config on every model request, including compaction and `dynamic_enabled=false`. Reload during generation affects that final. Started delivery keeps chosen presentation while refreshing security/credentials. Real tool uses a current-config accessor.

Core: `Slack_config_types`, `Slack_long_reply_policy`, lightweight `Slack_reply_context` (presentation metadata/Lwt only), config/prompt plumbing, minimal tool placeholder. `Session_types` adds only an optional queued-final function over its existing item; `Session_core` invokes it or fallback. Integrations: `Slack_file_upload`, `Slack_canvas`, `Slack_reply_delivery`, `Slack_final_text`, `Slack_final_reply`, `Slack_canvas_tool`, extracted `Slack_message_transport`; all HTTP stays here.

Cycle audit: `Session_types → Agent → Prompt_builder → Slack_reply_context → Lwt`. Context cannot import Session/Agent/integrations. Queued-final helper lives downstream in `Session_core`; prompt code never imports it. `Slack_file_upload` uses low-level HTTP/JSON/Lwt and injected checked callbacks; it imports no standing-order module, so relay wrapper/dispatch → shared has no return edge. `Slack_final_reply → Session` and delivery/API modules stay integration-only. Extract top-of-`slack.ml` transport wrappers into `Slack_message_transport`, which cannot import `Slack`; `slack.ml` must shrink below its initial 1121 lines. New modules below 1000, hard limit 2000.

## Tool and prompt contract

Name `slack_canvas_create`, risk `Medium`, non-streaming. Exact schema:

```json
{"type":"object","properties":{"title":{"type":"string","minLength":1,"maxLength":120,"description":"Short canvas title (required)."},"markdown":{"type":"string","minLength":1,"maxLength":1048576,"description":"Complete Markdown write-up (required)."}},"required":["title","markdown"],"additionalProperties":false}
```

Exact description:

> Create a Slack canvas containing a complete Markdown write-up, share its link in the current Slack channel, and give that channel read access. Requires an authorized Slack channel turn; the destination comes from that turn. Use for long or structured answers and research reports. For short answers, reply normally. Supply title and markdown; on success use the returned permalink in a brief reply and do not repeat the write-up. The tool publishes a short title-and-link message to establish sharing. In DMs reply normally; Clawq attaches long answers as files. If canvas sharing fails, it returns an error so you can reply normally and let automatic fallback preserve the answer.

Success JSON `{"canvas_id":"F…","permalink":"https://…","shared_message_ts":"…"}` only after read grant. Carrier is escaped title plus labelled link. Failure cleans owned artifacts and returns an actionable error, never a fake canvas/file success. Automatic `file`/`off` do not disable deliberate channel tool use.

Exact DM/MPDM error: `slack_canvas_create works in channels; in a DM reply normally and clawq attaches long answers as a file.`

Outside Slack: `slack_canvas_create requires an authorized Slack turn. Reply in the current conversation, or run this tool from Slack; do not supply a channel ID or token.` Cached invocations still require verified active-turn/snapshot authorization; no network writes on rejection. Hide outside Slack via neutral metadata in core availability. Minimal placeholder, same schema/description: `slack_canvas_create is disabled in minimal build; use the full clawq binary.` Full startup replaces it. Add `researcher`/`documenter` allowlists, review others and run `gen-agents`.

Canvas-mode channel guidance:

> In Slack, final answers longer than {threshold} characters are automatically placed in a canvas, with a Markdown file fallback. Lead with a self-contained opening paragraph of at most 600 characters; that paragraph and the full-answer link appear in the conversation. For long or structured research write-ups, you may call slack_canvas_create with a title and complete Markdown. It shares a short link message here; then reply briefly with the result and link. Do not repeat the full write-up. Short answers should be ordinary replies.

DM/MPDM or file mode says long answers are attached as Markdown files and uses “file attachment”. DMs omit tool advice. Off mode omits automatic-overflow advice, retaining optional-tool advice only for eligible channels where available. Remove tool advice when room/template filtering hides it. No Slack guidance for non-Slack/session-less requests. Scope neutral per-turn metadata independently of huddle authority; prevent concurrent-turn leaks.

## Acceptance and documentation

Fake HTTP covers threshold/multibyte boundaries, opening blocks, minimal conversion, canvas success/cleanup, file/full-text fallback, one create despite later failures, in-process later-stage retry, DM/MPDM files, blanks, root/thread targets, status-only streams, relay/notices exclusion, follow-up finals, config validation/reload, guarded tool and minimal linking. Also cover lost/malformed completion responses after a successful share, unresolved completion with confirmed versus failed deletion, checked text failures (`ok:false` and non-2xx) for short/off/fallback, the legacy final injection, and buffered/streaming executor log/audit redaction including mixed raw batches. The plan names each Alcotest case. Accepted crash/restart/replay duplicates are documented, not tested as an idempotency guarantee.

Implementation updates connector/setup/config references, `docs/public/llms-full.txt` and both built-in prompts. Explain modes/scopes/fallback limits, known final-producer gaps and coordinator-only post-deploy checks. No product question remains for Max: R3 and R4 settle the former DM and publication questions.

## Review responses

All three Important findings from `runs/rescope-review.review.json` are accepted. Task 5 now covers executor logging/audit projections, malformed and denied calls, and mixed raw batches without changing execution/history. Task 3 distinguishes confirmed, definitely failed and unresolved file sharing using call-local IDs and bounded reconciliation/owned cleanup. Tasks 3–4 pass a checked injectable text adapter through delivery and every final closure, preserving the supported legacy injection. No finding is rejected; R1–R8 remain binding, with five implementation tasks and no new persistence or recovery machinery.
