# I065 Slack long-reply canvas implementation plan

> **For agentic workers:** Execute task-by-task using the writing-plans checklist format. This assignment is DESIGN only: change the spec/plan and make the requested documentation commit; do not implement these tasks or spawn subagents.

**Prerequisite:** Implementation starts only after P3 (`feat/huddle-standing-orders`) merges to master and this branch is rebased onto that master. The controller/integrator performs that prerequisite in a later assignment; this DESIGN step does not rebase, merge or push. Task 1 extracts the actual merged P3 upload code before I065 adds any upload caller.

**Goal:** Readable final Slack answers: opening paragraph plus read-only channel canvas, DM/MPDM file-only overflow, exact file/full-text fallback, explicit channel research tool.

**Architecture:** Pure policy/config and presentation context in core; checked network adapters and final delivery in integrations. In-process create-once state only. Existing native streams remain status-only. A separate optional queue-final callback keeps notices/progress outside delivery.

**Tech Stack:** OCaml 5.1, Lwt, Yojson.Safe, existing Slack transport/policy checks, Alcotest fake HTTP, dune and ocamlformat 0.28.1 through the serialized remote gate.

**Binding inputs:** `.superpowers/sdd/2026-11-05-slack-canvas-replies/scope-ruling.md` (R1–R8) overrides requirements and prior design; `docs/superpowers/specs/2026-11-05-slack-long-replies-canvas-design.md` gives API citations and full behavior.

## Global Constraints

- Five implementation tasks below. Do not add persistence or change model/recovery semantics for delivery. One call attempts create once and reuses known ID for bounded later-stage retries. Crash/restart/replay duplicates are accepted; do not test cross-crash/replay idempotency.
- Code-point threshold strictly greater than **2500** by default; equality stays in chat. Modes **canvas|file|off**; threshold **1..40000**. Summary **600** scalars including ellipsis; title **120**. Preserve B845 and original fallback bytes. Minimal Markdown conversion only outside fences; Slack rejects unsupported structures. Conservative canvas ceiling **1048576 bytes/scalars**, without table/HTML/heading validators.
- DMs and MPDMs always use file for automatic overflow (except `off`); tool returns the spec's exact DM error. No user grants/member enumeration. Channel order is create → permalink → carrier → read grant; failure cleans owned canvas/carrier then file/text fallback.
- All HTTP and real tool code belong to `clawq_runtime_integrations`. Core includes policy/config, low-level presentation context, optional queued-final callback and schema-compatible minimal placeholder. Add modules to the correct explicit `src/dune` lists.
- Cycle rule: **Session_types → Agent → Prompt_builder → Slack_reply_context → Lwt**. Context imports no Session/Agent/integration module. Shared upload imports no standing-order module. Extracted transport imports no `Slack`. Keep new modules below 1000 lines (hard 2000); shrink `slack.ml` below its initial 1121.
- Every build/test/format/generation command uses `/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh`, sequentially. No local dune or parallel gate commands. The gate isolates HOME, serializes remote builds and caps jobs. Temporary test data stays under test temp roots.
- No live Slack API, credentials inspection, `~/.clawq`, attn, other-worktree writes, push/merge/rebase or subagents. P3 worktree is read-only reference. Stage only explicit task paths, including individually inspected generated files. Register new suites in `test/dune` and `test/test_main.ml`.
- Recheck current authorization/allowlist/private policy/egress and credentials before each HTTP operation. Revocation/cancellation stops writes, including fallback. Never log tokens, upload URLs, content, titles, summaries, raw HTTP bodies or HTTP exception strings.
- Preserve notifier/progress/relay behavior and deferred/suppressed output. Cover required final paths; direct background/resume and non-native queued finals other than follow-ups remain the spec's known gaps. No new product decision is pending Max.

## Review Focus

| Risk / ruling | Named test and required evidence |
| --- | --- |
| P3 extraction weakens policy or moves completion reservation (R6) | Task 1 `shared upload preserves relay fence`: limiter → fresh policy → synchronous reservation → one HTTP; refused reservation makes zero completion calls. Existing P3 upload/dispatch-retry/relay suites stay green. |
| DM canvas grants reappear (R3) | Task 3 `a DM long answer is attached as a file`: DM and MPDM issue zero canvas calls or recipient enumeration for grants, exact original bytes/parent in upload. Task 5 `dm tool gives file guidance`: exact error, zero writes. |
| Later-stage failures create again (R1/R8) | Task 3 `one delivery creates at most one canvas when a later step fails` and `later step retry reuses known canvas`: one create despite permalink/grant retry/failure; known ID reused. No restart guarantee. |
| Sharing precedes grant; cleanup drops answer (R4) | Task 3 `share then grant failure cleans and falls back`: exact operation order, read-only channel grant, canvas delete/carrier edit or delete attempted, original file delivered even if cleanup fails. |
| Threshold/summary or over-validation changes content (R5/R7) | Task 2 `threshold equality stays in chat`, `unicode fenced opening is capped`, `markdown only adapts tokens outside fences`; assert byte equality of unsupported structures and exact fallback. |
| Progress intercepted or answer shown twice (R2) | Task 4 `native long final never streams body`, `relay and notices bypass overflow`, `followup idle and queued finals`: one final presentation; zero body in stream operations; acknowledgements/progress make no artifacts. |
| Stale config or prompt creates wrong behavior (R7) | Tasks 2/3 `reload during generation changes final policy`, Task 5 `guidance rebuild follows current config`: runtime updates affect next final/request, including compaction/static prompt; active delivery does not restart. |
| Dependency cycle / minimal linkage (R7) | Task 5 `minimal tool is disabled` plus gate `minimal`: placeholder works, full tool replaces it, no core import of integrations or prompt import of Session_types. |

## Task 1: extract P3 upload-v2 primitives for both callers

**Files**

- Create `src/slack_file_upload.ml`, `test/test_slack_file_upload.ml` (suite `slack_file_upload`).
- Modify merged P3 `src/slack_standing_order_upload.ml`, `src/slack_standing_order_dispatch.ml`, `src/dune`, `test/dune`, `test/test_main.ml`.
- Extend existing `test/test_slack_standing_order_upload.ml`, `test/test_slack_standing_order_dispatch_retry.ml` only for extraction regressions. Keep `Slack_standing_order_slack.make`/`preflight`/selector restrictions unchanged.

**Interfaces — exact shared contract from the spec**

```ocaml
(* Slack_file_upload: integration-only; no standing-order import *)
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

P3 raw types become manifest aliases to shared types so existing injected fixtures compile. Existing `Slack_standing_order_upload.post` signature remains:

```ocaml
val post : db:Sqlite3.db -> current_config:(unit -> Runtime_config.t) ->
  snapshot:Access_snapshot.t -> channel_id:string -> audit:string ->
  ?http:Slack_file_upload.raw_http -> ?limit:(unit -> unit Lwt.t) ->
  ?before_send:(unit -> (unit, Slack_standing_order_slack.error) result) ->
  uri:string -> body:string -> unit ->
  (unit, Slack_standing_order_slack.error) result Lwt.t
```

Do not change P3 callers' public API. `authorize` delegates to `SO.preflight ~scope:Standing_order ~sensitive_uri:true` after limiter, using actual URI/current config and returning unit. Shared `post` runs `before_send` synchronously immediately before raw HTTP, without an intervening yield.

**Test-first steps**

- [ ] Register suite and write `upload v2 uses byte length and parent`: allocation sends `String.length` bytes through checked GET; raw POST contains exact bytes, no bot authorization; completion has channel, optional parent, initial comment. `completion rejects empty or duplicate ids` makes zero calls on invalid input.
- [ ] Write `upload URL pin and redaction survive extraction`: reject non-HTTPS, wrong host, userinfo, explicit port/fragment, control characters and backslash; 302 is an error with no redirect. Audit/HAR/log capture contains no capability, token or body. Retain existing P3 2xx behavior.
- [ ] Write `shared upload preserves relay fence`: fresh config/snapshot/allowlist/egress checks after limiter; strict-audit or `before_send` refusal means zero socket calls. Assert completion reservation executes once and remains synchronous. Keep P3 tests for pause, cancellation and ambiguous completion unchanged.
- [ ] Run red. Move P3 raw transport/pinning to shared module. In `Slack_standing_order_dispatch.deliver`, replace per-file allocation/parse with `allocate`, retaining ID save/duplicate checks; byte upload goes through compatibility wrapper. Replace batch completion with `complete`, existing checked closure, `thread_ts:None`, same IDs/comment. Preserve reservation/evidence/reconciliation logic.
- [ ] In the compatibility wrapper unwrap `Remote e` unchanged; map invalid response/transport to `SO.Transport`, invalid URL to `SO.Egress_denied "upload"`, status to `SO.Http_status`. In dispatch allocation, local validation errors instead keep the existing terminal `upload_allocation_invalid` branch; remote errors keep `precomplete_error "upload_allocation_failed"`. Add `malformed allocation stays terminal` asserting no new transient retry and the original reason. Checked API closures retain method/allowlist gates. Shared module never imports `Slack_standing_order_slack`; this prevents a wrapper/shared cycle.
- [ ] Run green and P3 regression gates, full tests and optimized/minimal builds for the extraction; stage listed paths only.

**Verification (sequential)**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_standing_order_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_standing_order_slack
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_standing_order_dispatch_retry
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_standing_order_relay
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh all
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh opt
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `refactor(slack): share P3 upload v2 primitives`

## Task 2: pure overflow policy, live config and neutral prompt metadata

**Files**

- Create `src/slack_long_reply_policy.ml`, `src/slack_reply_context.ml`, `test/test_slack_long_reply.ml` (suite `slack_long_reply`).
- Modify `src/slack_config_types.ml`, `src/runtime_config_types.ml`, `src/config_loader_channels.ml`, `src/runtime_config_json_sections.ml`, `src/config_set.ml`, `src/config_tree.ml`, `src/config_search.ml`, `src/config_show.ml` where enumeration needs it; `src/dune`, `test/dune`, `test/test_main.ml`.
- Extend `test/test_config_loader.ml`, `test/test_config_set.ml`; update explicitly identified Slack-config record fixtures (including `test/test_slack.ml`) with the new default field only.

**Interfaces**

```ocaml
(* Slack_config_types; slack_config adds long_reply : long_reply *)
type long_reply_mode = Canvas | File | Off
type long_reply = { mode : long_reply_mode; threshold : int }
val default_long_reply : long_reply
val long_reply_of_json : Yojson.Safe.t -> long_reply
val long_reply_to_json : long_reply -> Yojson.Safe.t
val validate_long_reply : Yojson.Safe.t -> (unit, string) result
(* Slack_long_reply_policy *)
val code_points : string -> (int, string) result
val overflows : Slack_config_types.long_reply -> string -> bool
val summary : string -> string
val prepare_markdown : string -> (string, string) result
val automatic_title : source_ts:string option -> string
(* Slack_reply_context: presentation only; depends on Lwt, not config/Session *)
type conversation = Channel | Direct | Group_direct | Unknown
type t = { connector : string; conversation : conversation }
val with_context : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val current_context : unit -> t option
```

`prepare_markdown` errors only for invalid UTF-8/size ceiling, not Markdown structures. Context is neither a final dispatcher nor a source of tool authority. Its dependency graph ends at Lwt; safe for Prompt_builder and Agent_profile upstream of Session_types.

**Test-first steps**

- [ ] Write `threshold equality stays in chat`: 2499/2500 text, 2501 overflow; repeat with multibyte scalars. `blank and invalid UTF8 bypass artifacts`: no empty writes/replacement characters.
- [ ] Write `unicode fenced opening is capped`: CRLF, leading headings/lists/code, blank line within tilde/backtick fences, no paragraph break, 600 vs 601 scalars, 599 plus ellipsis, Slack escaping measured after cap. Original body is unchanged.
- [ ] Write `markdown only adapts tokens outside fences`: all mention/link/broadcast conversions; fences byte-identical. HTML, h4 and 301-cell tables pass through unchanged to Slack. Byte/scalar ceiling rejection uses exact-file fallback later.
- [ ] Add loader cases `long reply defaults fields independently`, `unknown mode warns and defaults`; validation cases `long reply validates leaves object and parents`: accept 1/40000, reject 0/40001, null/float/bool/unknown keys, including nested `channels`/`channels.slack` updates. Errors show key/example; logs do not echo untrusted values.
- [ ] Run red; implement parsing/serialization/set/show/search/tree and current-config behavior. Add `reload during generation changes final policy` using `Session.update_config` (no production config writes); assert mode/threshold change without restart. No cached derived state.
- [ ] Run green, quick, formatting and minimal gates. Stage explicit new/modified fixture paths after inspection.

**Verification**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_long_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_loader
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_set
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): add long reply policy and live configuration`

## Task 3: channel canvas delivery and DM file fallback

**Files**

- Create `src/slack_canvas.ml`, `src/slack_reply_delivery.ml`, `test/test_slack_canvas.ml` (suite `slack_canvas`), `test/test_slack_reply_delivery.ml` (suite `slack_reply_delivery`).
- Modify `src/dune`, `test/dune`, `test/test_main.ml`. Consume Task 1 shared upload; do not add another upload transport.

**Interfaces**

```ocaml
(* Slack_canvas: integration-only checked API primitives *)
type error = Policy_denied | Transport of string
type call = string -> (string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, error) result Lwt.t
val create : call:call -> title:string -> markdown:string ->
  (string, error) result Lwt.t
val permalink : call:call -> canvas_id:string -> (string, error) result Lwt.t
val grant_read : call:call -> canvas_id:string -> channel_id:string ->
  (unit, error) result Lwt.t
val delete : call:call -> canvas_id:string -> (unit, error) result Lwt.t
(* Slack_reply_delivery *)
type target = {
  session_key : string; channel_id : string; thread_ts : string option;
  requester : string; source_ts : string option;
  conversation : Slack_reply_context.conversation;
}
type deps = {
  current_config : unit -> Runtime_config.t;
  authorize : target -> (unit, Slack_canvas.error) result Lwt.t;
  call : target -> Slack_canvas.call;
  upload : target -> summary:string -> body:string ->
    (unit, Slack_canvas.error) result Lwt.t;
  post_text : target -> string -> (unit, Slack_canvas.error) result Lwt.t;
}
type artifact = { canvas_id : string; permalink : string; shared_message_ts : string }
type outcome = Canvas_sent of artifact | File_sent | Text_sent | Suppressed
val make_deps : manager:Session.t -> deps
val publish_canvas : deps:deps -> target:target -> title:string ->
  markdown:string -> carrier:string -> (artifact, Slack_canvas.error) result Lwt.t
val deliver : deps:deps -> target:target -> body:string ->
  (outcome, Slack_canvas.error) result Lwt.t
```

`make_deps` supplies checked API calls and the file wrapper using shared `allocate/post/complete`, current config and original target. `call` binds channel/parent, enforces method/parameter selectors and policy per request, not only at construction. New API requests reuse P3's no-capture `Slack_standing_order_slack.api_transport` socket adapter (not its standing-order-scoped `make`), retaining Slack_transport fixture injection. I065's raw-upload authorization checks current allowlist/private policy/snapshot and strict audited egress against the actual sensitive upload URI. Legacy and native authorization stays intact, including existing membership checks; there is no new enumeration for grant recipients. Metadata refresh distinguishes DM/MPDM/private; missing canvas eligibility never grants permission. `upload` uses original bytes, `.md` filename, summary comment and exact parent; shared errors map to safe classes. No integration API is imported by core. The new delivery → P3 socket-adapter dependency has no return edge.

`publish_canvas` does one create → permalink → non-empty carrier plus labelled link → read grant and owned cleanup on failure. `carrier` is the escaped opening paragraph (automatic) or escaped title (tool); this helper appends the trusted link. `deliver` selects policy/file/text and calls that helper only for eligible channel overflow. All state is call-local; bounded later-stage retries reuse ID. A denied security check stops every fallback.

**Test-first steps**

- [ ] Write `canvas success posts summary plus link`: exact API order, standalone create without channel, permalink from `files.info`, grant `read` to one channel, same root/thread and no broadcast/unfurl. Require grant before success.
- [ ] Write `share then grant failure cleans and falls back`: test failure at create/permalink/post/grant; known canvas deletion, carrier edit (or delete) before exact file fallback. Cleanup failure still delivers the answer. File failure invokes exact full-text sender; all-send failure never reports success.
- [ ] Write `one delivery creates at most one canvas when a later step fails`; `later step retry reuses known canvas`: injected 429/transient lookup/grant failures reuse ID; unknown create outcome never retries create. Completion is never repeated on an ambiguous ticket outcome.
- [ ] Write `a DM long answer is attached as a file`: parameterize IM/MPIM, root/thread, canvas/file modes. Zero canvas calls/new recipient enumeration for grants (existing authorization remains); one exact `.md`, summary comment and original destination. `off and short stay text` covers equality and DM off. `blank final sends nothing` covers B845.
- [ ] Write `canvas rejected markdown falls back unchanged`: Slack rejects HTML/deep-heading/table content after one attempted create; file bytes exactly original. `policy revocation stops fallback` tests refreshed credentials/allowlist/private policy/egress after each await; cancellation causes no late upload/post.
- [ ] Write `reload after entry keeps chosen presentation` and `delivery logs redact secrets`: injected sentinels for token/upload URL/answer/title/summary/raw errors never appear in logs/audit/HAR. Run red, implement, then green/quick/format gates.

**Verification**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_delivery
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): deliver long channel answers as canvases and DMs as files`

## Task 4: extract transport and wire actual final call sites

**Files**

- Create `src/slack_message_transport.ml`, `src/slack_final_reply.ml`, `test/test_slack_reply_paths.ml` (suite `slack_reply_paths`).
- Modify `src/slack.ml`, `src/slack_thread_execution.ml`, `src/daemon_inbound_replay.ml` (native final send only), `src/connector_dispatch.ml`, `src/session_types.ml`, `src/session_core.ml`, `src/session_turn.ml` (queue final dispatch only), `src/session_agents.ml`, `src/dune`, `test/dune`, `test/test_main.ml`.
- Update explicit `Connector_dispatch.dispatch_env` constructors in `src/discord.ml`, `src/telegram.ml` and compiler-identified fixtures with `send_final=None`, preserving their behavior. Existing `test/test_slack.ml` remains a regression suite.

**Interfaces**

```ocaml
(* Session_types: field mutable queued_final_sender : queued_final_sender option *)
type queued_final_sender = key:string -> message:queued_message ->
  fallback:(string -> unit Lwt.t) -> string -> unit Lwt.t
(* Session_core; export through existing Session facade *)
val set_queued_final_sender : Session_types.t ->
  Session_types.queued_final_sender option -> unit
val send_queued_final : Session_types.t -> key:string ->
  message:Session_types.queued_message -> fallback:(string -> unit Lwt.t) ->
  string -> unit Lwt.t
(* Slack_final_reply: integrations *)
val install : Session.t -> unit
val send : manager:Session.t -> target:Slack_reply_delivery.target ->
  string -> unit Lwt.t
val send_queued : manager:Session.t -> key:string ->
  message:Session_types.queued_message -> fallback:(string -> unit Lwt.t) ->
  string -> unit Lwt.t
(* Slack_message_transport: extract existing wrappers; no import of Slack *)
type t = {
  post : text:string -> unit Lwt.t;
  post_with_id : text:string -> string Lwt.t;
  edit : ts:string -> text:string -> unit Lwt.t;
  delete : ts:string -> unit Lwt.t;
  reaction : method_:string -> timestamp:string -> emoji_name:string -> unit Lwt.t;
}
val make : manager:Session.t -> route:Slack_thread_route.t option ->
  channel_id:string -> thread_ts:string option ->
  legacy_post:(bot_token:string -> channel_id:string -> text:string -> unit Lwt.t) -> t
```

Extract status-notifier construction with transport wrappers as needed, preserving existing send injection and blank checks. For legacy posts with an explicit parent, retain the existing threaded API; `legacy_post` preserves the unthreaded injection seam. Other legacy operations use existing `Slack_api` helpers/current credentials. Native operations retain `Slack_thread_api` checks.

`dispatch_env` gains `send_final : (string -> unit Lwt.t) option`. Exact additive signatures (all default `send_final` to `send_reply`):

```ocaml
(* Connector_dispatch *)
val dispatch_followup : session_mgr:Session.t -> key:string ->
  connector_name:string -> channel_id:string -> user_id:string ->
  ?channel_name:string -> ?channel_type:string -> ?sender_name:string ->
  ?message_id:string -> is_admin:bool -> send_reply:(string -> unit Lwt.t) ->
  ?send_final:(string -> unit Lwt.t) -> Slash_commands.followup_action -> unit Lwt.t
(* Session_agents; Session_types.t avoids importing the Session facade *)
val agent_invoke_turn : Session_types.t -> ?parent_key:string ->
  ?debug_notify:(string -> unit Lwt.t) -> agent_name:string -> prompt:string ->
  send_reply:(string -> unit Lwt.t) -> ?send_final:(string -> unit Lwt.t) -> unit -> unit
val delegate_turn : Session_types.t -> ?parent_key:string ->
  ?debug_notify:(string -> unit Lwt.t) -> ?agent_name:string -> prompt:string ->
  send_reply:(string -> unit Lwt.t) -> ?send_final:(string -> unit Lwt.t) -> unit -> unit
val fork_and_run : Session_types.t -> parent_key:string ->
  ?debug_notify:(string -> unit Lwt.t) -> ?agent_name:string -> prompt:string ->
  send_reply:(string -> unit Lwt.t) -> ?send_final:(string -> unit Lwt.t) -> unit -> unit
```

`install` sets the core queued-final callback during existing `Slack_thread_execution.install`. `send_queued` handles verified native Slack routes and actual queued requester/owner; it also handles legacy Slack items with `deferred_followup=true`, deriving channel/requester from that item and retaining historical root targeting (`thread_ts=None`). Other items invoke fallback. Call it at Session_turn's post-execution final notify site, after existing suppression checks. No use of progress notifier text as a final classifier. `send` maps failed delivery to the existing safe transport failure path and returns only after success/suppression. Required finals mark sent after successful dispatch; preserve existing retain/error semantics.

**Test-first steps**

- [ ] Write `native long final never streams body`: fake multi-delta answer >2500; start/maintenance/stop contain status only, no answer append, one final canvas carrier. Repeat with unavailable native stream, rotation and `off`.
- [ ] Write `before drain and result deliver once`: public Slack handler, both result branches and sent marker; original root/thread preserved for posted/native/private/DM/MPDM targets, no broadcast. Capture target before huddle context clears, reauthorize at send.
- [ ] Write `native queued and replay finals use sender`: different queued requester from active turn, existing native route/owner checks, exact root and one presentation per invocation; replay routes through final sender. This is routing coverage, not a duplicate-prevention assertion across replay/restart.
- [ ] Write `followup idle and queued finals`: both idle branches plus busy native and legacy follow-ups deliver long answers; native root and historical legacy root are preserved. Queue/append acknowledgements/progress produce zero artifacts. `agent document notices stay ordinary` covers invoke/delegate/fork notices/errors/debug versus completed answers. `live autonomous final uses sender` covers both Slack on_response callbacks.
- [ ] Write `relay and notices bypass overflow`: long P3 relay, thinking, progress, pause receipt and card all bypass final delivery. Run existing `slack_empty_reply` and `slack_relay_guard` suites. `non native queued final preserves fallback` uses a non-follow-up item and proves the listed gap retains historical behavior.
- [ ] Run red, extract transport, wire the listed sites only. Initialize optional callback to None; non-Slack callers keep prior sender. Review every modified interface: Session_types contains only existing queued types and callback, never Slack integration types; Session_core does not import Slack_final_reply; extracted transport does not import Slack.
- [ ] Run green, quick/full/optimized/minimal/format gates. Verify `slack.ml` shrank below 1121 lines; new modules <1000. Record direct background/resume and non-native queue gaps in docs; do not expand this task to shared recovery or notification changes.

**Verification**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_empty_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_relay_guard
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh all
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh opt
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): route final replies through local overflow delivery`

## Task 5: guarded canvas tool, per-request guidance, templates and operator docs

**Files**

- Create `src/slack_canvas_tool.ml`, `test/test_slack_canvas_tool.ml` (suite `slack_canvas_tool`).
- Modify `src/tools_builtin.ml`, `src/daemon_startup.ml` (registry replacement), `src/room_tool_availability.ml`, `src/agent_profile.ml`, `src/prompt_builder.ml`, `src/slack.ml`, `src/slack_thread_execution.ml` (neutral context only), `src/dune`, `test/dune`, `test/test_main.ml`, `test/test_prompt_builder.ml`.
- Modify `docs/builtin-agent-prompts/researcher.md`, `docs/builtin-agent-prompts/documenter.md`; regenerate `src/agent_template_builtins_research.ml` and explicitly inspect/list any other generated file changed by `gen-agents`.
- Modify `docs/src/content/docs/channels.mdx`, `docs/setup-guide.md`, `docs/src/content/docs/configuration.mdx`, `docs/public/llms-full.txt`.

**Interfaces**

```ocaml
(* Tools_builtin: core placeholder with spec's schema/description *)
val slack_canvas_create : unit -> Tool.t
(* Slack_canvas_tool: integration implementation, replaces placeholder *)
val make : current_config:(unit -> Runtime_config.t) -> Tool.t
val register : current_config:(unit -> Runtime_config.t) -> Tool_registry.t -> unit
(* Prompt_builder: pure helper, reads neutral metadata, no Session import *)
val slack_reply_guidance : context:Slack_reply_context.t option ->
  config:Runtime_config.t -> tool_available:bool -> string option
(* Room_tool_availability: composes existing template/authz filtering *)
val slack_canvas_available : context:Slack_reply_context.t option -> bool
```

Tool invocation uses existing `Slack_huddle_context.resolve ~tool_name` and snapshot policy, validates arguments/title at runtime, checks typed conversation metadata and calls Task 3 `publish_canvas` only in channels. It returns the exact DM or outside-Slack error without writes; explicit failure returns an actionable error after cleanup, not file success. No new tool argument selects token/channel/parent. `file`/`off` do not disable explicit channel use. Use `Tool_registry.replace` at full startup and a current-config accessor, respecting existing tools_enabled/filtering. No changes to tool invocation machinery.

Scope `Slack_reply_context.with_context` around verified Slack execution using its connector and typed conversation metadata, including each native queued item rather than the preceding turn. Do not infer DM kind from ID prefixes. The existing `Agent_1_context.build_messages` already rebuilds Prompt_builder per request; Prompt_builder reads only this neutral context. `Session.update_config` already refreshes active agents' config. When verified room metadata is unavailable use Unknown and avoid promising channel canvas behavior. Build guidance unconditionally with respect to `dynamic_enabled`, from current request config and actual filtered tool set, including after compaction. No Prompt_builder → Session_types/Agent/Slack_huddle_context edge. Session-less/non-Slack context gets no advice. This metadata cannot authorize the tool.

**Test-first steps**

- [ ] Write `outside slack gives actionable error`, `dm tool gives file guidance`, `arguments cannot select destination`: exact errors/strict schema, no writes. `stale snapshot cannot publish` tests cached invocation and injection invalidation. `channel tool shares read only` checks title/link carrier, same channel/parent and success JSON only after grant.
- [ ] Write `canvas title validates Unicode and length`: trimmed non-empty title; 120 scalars accepted, 121/invalid UTF-8/blank rejected without writes. `failed tool cleans and returns error` never returns fake link/file success. One invocation never repeats create.
- [ ] Add prompt cases `guidance rebuild follows current config`, `DM guidance promises files`, `hidden tool omits advice`, `static and compacted prompts retain guidance`, `non Slack has no guidance`, `parallel context does not leak`. Assert exact threshold/mode, current config after reload/compaction and no tool advice when filtered.
- [ ] Add `minimal tool is disabled`: identical schema/description and exact placeholder error. Run red; implement full registry replacement, filtering and prompt helper. Review every built-in allow/disallow list; grant researcher/documenter only by default. Generate/inspect built-ins and check regeneration.
- [ ] Update operator docs: defaults/modes/validation/live reload, strict code-point threshold, capped opening paragraph, channel-only tool/canvas, DM/MPDM files, standalone/share-before-read behavior, fallback limits, accepted crash/replay duplicates and known final-producer gaps. Explain scopes/reinstall, separate Slack API and upload-host egress, no public sharing, redacted logs and no guaranteed unfurl layout.
- [ ] Record coordinator-only post-deploy checks: root/thread/private channel read-only canvas; DM/MPDM file in originating conversation; summary/link rendering, brief no-access interval, file/full-text fallback. Implementers never perform live checks.
- [ ] Run acceptance commands sequentially. Confirm all new suites are listed, P3 regressions green, generated agents clean, core/minimal graph acyclic, size limits met and explicit diff contains no unrelated/generated drift. Stage exact changed files and commit.

**Verification**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune exec -j 2 test/test_main.exe -- list'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas_tool
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite prompt_builder
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): add channel canvas tool guidance and documentation`

## Design closeout

R1–R8 replace the prior design's broader guarantees. The shared upload interface and P3 call sites were checked read-only against P3 code; each new interface above has an explicit dependency direction. Direct background/resume and non-native queued finals other than follow-ups remain documented gaps under R2. R3/R4 settle the former Max questions; no question only Max can answer remains. This DESIGN step runs document checks only; the implementation gates above are future verification, not claimed results.
