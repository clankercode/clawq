# I065 Slack long-reply canvas implementation plan

> **For agentic workers:** Execute this plan task-by-task using the superpowers writing-plans checklist structure. This run is design-only. Implementation requires a subsequent implementation assignment; the current instruction prohibits subagents. Preserve that restriction unless Max changes it.

**Goal:** Deliver Clawq's long final Slack answers as an opening paragraph plus a read-only canvas link, with lossless Markdown-file/plain-message fallback and an explicit research canvas tool.

**Architecture:** Pure policy/config and a neutral final-delivery callback in core; policy-checked Slack canvas/upload adapters, a SQLite delivery journal and final-only routing in integrations. Buffer answer deltas while existing native streams show status. A durable create-attempt fence enforces at-most-one canvas even when Slack's response is lost.

**Tech Stack:** OCaml 5.1, Lwt, Yojson.Safe, SQLite3, existing Slack_transport/Policy_http_client, Alcotest fake HTTP, dune and ocamlformat 0.28.1 through the serialized remote gate.

**Spec:** `docs/superpowers/specs/2026-11-05-slack-long-replies-canvas-design.md` (binding behaviour and public Slack API citations); approved requirements at `.superpowers/sdd/2026-11-05-slack-canvas-replies/requirements.md`.

## Global Constraints

- Default threshold **2500**, strictly greater-than, Unicode scalar values. Modes **canvas|file|off**, default **canvas**; threshold validation **1..40000**. Opening block summary at most **600** scalar values, title at most **120**. Canvas payload conservative ceiling **1048576 bytes and scalar values**, tables **300 cells**. Exact extraction/conversion rules are in the spec.
- Core modules: existing `Slack_config_types`, new **Slack_reply_types**, **Slack_reply_context**, **Slack_long_reply_policy**, **Session_reply_lifecycle**, **Session_final_reply**. Integrations modules: new **Slack_canvas**, **Slack_file_upload**, **Slack_reply_store**, **Slack_reply_delivery**, **Slack_canvas_tool**, **Slack_message_transport**. Add module names to the correct explicit lists in `src/dune`. No HTTP/network dependency enters `clawq_runtime_core`; the real tool belongs in `clawq_runtime_integrations`. Core's registered placeholder returns **disabled in minimal build**.
- Soft file-size limit **1000 lines**, hard **2000 lines**; each new module below **1000**. `src/slack.ml` starts at **1121** and must shrink when touched. Extract transport wrappers rather than append policy to it. Avoid Session↔Slack cycles.
- All build/test/format/generation commands use **`/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh`**. Never run dune locally or in parallel. The gate isolates HOME and serializes remote work; no direct live Slack, local daemon, token, `~/.clawq`, attn, other worktree, push, merge or rebase. Only fake HTTP with injectable zero-delay clocks. Temporary SQLite/config data stays under test temp roots.
- Register every new suite in **both `test/dune` and `test/test_main.ml`**, even when adding more cases to a suite created in an earlier task. Stage only the task's explicit files; list generated `.ml` paths explicitly after inspecting their diff. No `git add -A`.
- Final-only semantics include native/legacy, root/thread, DM/MPDM, queue replay and autonomous/agent-command finals. Existing string notifiers remain progress/notice senders. Preserve B845 blank/suppression checks, current authorization, private-channel policy, owner/stop rules and response-sent semantics.
- Public docs require link sharing before access-set and user grants for DM/MPDM. Do not fabricate a DM channel grant, permalink or Slack idempotency key. A failed permission operation is a canvas failure. Coordinator verifies live permissions after deployment; implementers never call live Slack.
- Upload v2 is **absent in this checkout**. Task 2 creates its adapter; do not depend on future standing-order code. If it appears before implementation, reconcile its actual contract and reuse only a policy-safe implementation.
- Two spec entries are **Default pending Max** (DM membership lifetime; share-before-read interval). Implement the stated defaults unless Max directs otherwise, and retain coordinator acceptance checks.

## Review Focus

| Input most likely to bite | Owning task and proof |
| --- | --- |
| Slack access API rejects DM/MPDM IDs and requires prior sharing; successful create is insufficient | Task 2 `dm uses users after share`, `mpdm incomplete members falls back`, `grant failure is not success`; assert operation order and exact recipients. Task 3 proves recovery of carrier/fallback. |
| `turn_verified_stream` is provider streaming; native streams currently contain status only | Task 4 `native long final never streams body`; inspect start/append/stop/post calls across live, drain and replay; assert one final and no answer body in stream calls. |
| Crash after remote create but before saving ID; durable replay regenerating an answer or using another turn's identity | Task 3 `lost create response never creates twice`, Task 4 `replay resumes stored final before model`; assert one create, zero replay model calls, stable request slot despite different generated text/attempt count. |
| Captured config and mixed progress/final notifiers; private-channel/egress revocation during fallback | Task 1 `reload changes next final`, Task 3 `policy revocation stops fallback`, Task 4 `relay and notices bypass overflow`; assert effective config and no unauthorized writes. |
| Slack Markdown/byte limits and unsupported assumed upload implementation | Task 1 `unicode boundary and fenced opening`, Task 2 `upload v2 uses byte length and parent`, Task 3 `unsupported markdown goes to exact file`; assert original byte equality, form encoding and no lossy canvas. |

## Task 1: pure policy, config and neutral delivery contract

**Files**

- Create: `src/slack_reply_types.ml`, `src/slack_reply_context.ml`, `src/slack_long_reply_policy.ml`, `src/session_reply_lifecycle.ml`, `src/session_final_reply.ml`, `test/test_slack_long_reply.ml` (suite `slack_long_reply`).
- Modify: `src/slack_config_types.ml`, `src/runtime_config_types.ml`, `src/config_loader_channels.ml`, `src/runtime_config_json_sections.ml`, `src/config_set.ml`, `src/config_tree.ml`, `src/config_search.ml`, `src/config_show.ml` where needed for actual enumeration; `src/session_types.ml`, `src/session_core.ml`, `src/dune`, `test/dune`, `test/test_main.ml`.
- Test/fixture modifications: `test/test_config_loader.ml`, `test/test_config_set.ml`, `test/test_slack.ml` and every explicit `slack_config` record constructor found by compiler/search; add `long_reply` defaults, without unrelated fixture edits.

**Interfaces — consumes JSON/current config; produces typed policy and a core callback seam**

```ocaml
(* Slack_config_types additions *)
type long_reply_mode = Canvas | File | Off
type long_reply = { mode : long_reply_mode; threshold : int }
val default_long_reply : long_reply
val long_reply_of_json : Yojson.Safe.t -> long_reply
val long_reply_to_json : long_reply -> Yojson.Safe.t
val validate_long_reply : Yojson.Safe.t -> (unit, string) result
(* Runtime_config_types.slack_config adds long_reply : Slack_config_types.long_reply *)

(* Slack_reply_types: no dependency on Session or integration modules *)
type context = {
  connector : string; session_key : string; channel_id : string;
  thread_ts : string option; requester : string option;
  request_id : string; ordinal : int;
}
type target = {
  team_id : string; app_id : string; workspace_url : string;
  channel_id : string; thread_ts : string option;
  requester : string; context : context;
}
type installation = { team_id : string; app_id : string }
type slot = Final of int
type journal_key = {
  installation : installation; session_key : string;
  request_id : string; slot : slot;
}
type operation_id = string
(* Digests do not identify operations; explicit calls bind to the active Final slot. *)
type artifact = { canvas_id : string; permalink : string; shared_ts : string }
type outcome = Canvas_sent of artifact | File_sent of string | Text_sent | Suppressed
type error = Policy_denied | Storage_unavailable | Cancelled | Failed of string
type sender = context -> string -> unit Lwt.t

(* Slack_long_reply_policy *)
val code_points : string -> (int, string) result
val summary : string -> string
val overflows : Slack_config_types.long_reply -> string -> bool
val prepare_markdown : string -> (string, string) result
val automatic_title : request_id:string -> string

(* Session_types.t adds mutable final_reply_sender : Slack_reply_types.sender option *)
(* Slack_reply_context: depends ONLY on Slack_reply_types and Lwt *)
val with_context : Slack_reply_types.context -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val current_context : unit -> Slack_reply_types.context option
(* Tool metadata is also low-level and scoped; no Session/Agent type dependency. *)
type invocation = { provider_call_id : string; sequence : int }
val with_invocation : invocation -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val current_invocation : unit -> invocation option

(* Session_reply_lifecycle: core, SQLite + low-level types; no Session/Agent/HTTP *)
val current_epoch : db:Sqlite3.db -> session_key:string -> int
val clear_session : db:Sqlite3.db -> session_key:string -> unit
(* Advance epoch and tombstone versioned journal tables synchronously; raises on DB failure. *)

(* Session_final_reply uses Session_types.t, never Session or Slack_canvas *)
exception Retained_delivery of Slack_reply_types.error
val send : Session_types.t -> context:Slack_reply_types.context ->
  fallback:(string -> unit Lwt.t) -> string -> unit Lwt.t
```

`Session_final_reply.send` invokes the installed sender for Slack, otherwise fallback; an absent sender also uses fallback. It does not classify arbitrary progress. The separate `Slack_reply_context` cell supplies verified presentation metadata for later prompt wiring. Initialize the field to `None`, and scope/clear metadata with `Slack_reply_context.with_context` and `Lwt.finalize`. Prompt_builder/Agent_profile must not import Session_final_reply or Session_types; compile the completed graph again in Task 5. Session_reply_lifecycle defines the epoch and tombstone SQL contract used by Task 3 (no HTTP); Task 4 wires the public clear paths. It must not imply tool authority (the tool separately resolves ingress/snapshot).

**Test-first steps**

- [ ] Register `slack_long_reply`; write `threshold equality stays in chat` (2499/2500 do not overflow; 2501 does), `unicode boundary and fenced opening` (multibyte scalars, CRLF, blank line inside fence, heading/list first block, no paragraph break, 599+ellipsis, whitespace and literal mentions), `markdown limits preserve original` (300/301 cells, code untouched, h4/HTML rejected, bytes/scalars ceiling).
- [ ] Write `long reply config round trips` (missing defaults; all three modes; unknown mode warns once with no raw value), `config set rejects malformed nested values` (leaf, whole object and parent-object routes; 0/40001/float/bool/null/unknown key; actionable examples), `reload changes next final` (swap manager config before final; sender reads new mode/threshold).
- [ ] Add `canvas mentions and links convert only outside code`: `<@U…>`, `<#C…|label>` and `<https://…|label>` convert to documented canvas Markdown outside fenced/inline code; code bytes remain untouched. `broadcast tokens remain literal` asserts `<!channel>`, `<!here>`, `<!everyone>` are escaped literal content and cannot expand mentions. Assert the original input bytes remain available for file/text fallback.
- [ ] Add `loader defaults invalid fields independently`: invalid threshold preserves valid mode, invalid mode preserves valid threshold, missing fields default individually, and non-object long_reply defaults both. Expect one sanitized warning per invalid supplied field (one object warning for malformed object), no warning for missing fields, no raw invalid values in logs.
- [ ] Run the new suite red. Implement pure policy and JSON codec/schema/serialization; add `channels.slack.long_reply` leaves and whole-object/ancestor validation in config_set. Use the existing runtime reload flow; do not cache policy in a connector closure.
- [ ] Implement/test `final callback is separate from progress` and `neutral context is restored`: callback called once for Slack final, non-Slack fallback once, a progress notifier untouched, nested context in Slack_reply_context unwinds on exception and cancellation, with no dependency on Session_types. Test lifecycle epoch advancement, missing-table no-op and failure propagation before wiring real clear callers.
- [ ] Run green and inspect the core module graph for references to Slack network modules. Commit only explicit files.

**Verification** (each command runs sequentially):

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_long_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_set
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_loader
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): add long-reply policy and live config`

## Task 2: policy-safe canvas and upload-v2 adapters

**Files**

- Create: `src/slack_canvas.ml`, `src/slack_file_upload.ml`, `test/test_slack_canvas.ml` (suite `slack_canvas`), `test/test_slack_file_upload.ml` (suite `slack_file_upload`).
- Modify: `src/slack_thread_api.ml` (safe reusable explicit-context checks, without weakening authorization), `src/slack_transport.ml` (injectable form/raw requests if missing), `src/dune`, `test/dune`, `test/test_main.ml`.

**Interfaces — consumes a server-resolved target and policy-checked transport; produces IDs/receipts, never user-facing raw errors**

```ocaml
(* Slack_canvas; request encapsulates current config + egress + audit *)
type call = string -> (string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, Slack_reply_types.error) result Lwt.t
type recipients = Channel of string | Users of string list
type canvas_unavailable = Too_many_recipients | Recipient_lookup_unavailable
(* Additional sanitized canvas-only reasons may be added; never encode policy denial here. *)
type canvas_eligibility = Eligible of recipients | Unavailable of canvas_unavailable
val resolve_target : manager:Session.t -> context:Slack_reply_types.context ->
  (Slack_reply_types.target, Slack_reply_types.error) result Lwt.t
val canvas_eligibility : manager:Session.t -> target:Slack_reply_types.target ->
  (canvas_eligibility, Slack_reply_types.error) result Lwt.t
val create : call:call -> title:string -> markdown:string ->
  (string, Slack_reply_types.error) result Lwt.t
val permalink : call:call -> target:Slack_reply_types.target -> canvas_id:string ->
  (string, Slack_reply_types.error) result Lwt.t
val set_read : call:call -> canvas_id:string -> recipients:recipients ->
  (unit, Slack_reply_types.error) result Lwt.t

(* Slack_file_upload; network methods remain in integrations *)
type ticket = { file_id : string; upload_url : string }
type transport = {
  api : verb:string -> encoding:[ `Form | `Json ] -> method_:string ->
    params:(string * Yojson.Safe.t) list ->
    (Yojson.Safe.t, Slack_reply_types.error) result Lwt.t;
  upload : uri:string -> body:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
}
val ticket : transport:transport -> filename:string -> length:int ->
  (ticket, Slack_reply_types.error) result Lwt.t
val put : transport:transport -> ticket:ticket -> body:string ->
  (unit, Slack_reply_types.error) result Lwt.t
val complete : transport:transport -> ticket:ticket ->
  target:Slack_reply_types.target -> summary:string ->
  (string, Slack_reply_types.error) result Lwt.t
```

`put` names the adapter operation; wire method is **POST**, not HTTP PUT. Callers own journalling/once-only decisions. `call` dispatches GET for files.info/metadata and POST for create/access/owned-canvas cleanup; the upload ticket method uses form encoding even though most other writes are JSON. Resolve current policy before every call, including rechecking metadata/membership before grants. Resolve legacy installation identity from auth.test plus bots.info when there is no verified envelope/native route; validate returned bot/app/team and never guess an app ID. Destination resolution establishes policy authorization independently of canvas eligibility. Bound DM/MPDM grant pagination; `Unavailable` retains the authorized target for file/text, and must never carry an oversized/partial Users list. >20 fully resolved human recipients selects `Too_many_recipients`. Incomplete canvas enumeration selects `Recipient_lookup_unavailable` only when all authorization checks independently succeeded; otherwise return Policy_denied and stop every outward write. Distinguish `is_mpim` from a private channel. Revalidate destination authorization before each fallback and membership immediately before grant. Use a closed method list and sanitized error codes. An injected fake exercises the same serialization and policy path as production.

**Test-first steps**

- [ ] Register suites and write `standalone create has no channel id` (exact title/document_content, no token in JSON; returns ID), `permalink requires matching private file` (files.info GET, returned permalink, missing scope/mismatched host/public-only result errors), `read channel grant` (one channel, `read`, no org/public/write).
- [ ] Write `dm uses users after share` and `mpdm incomplete members falls back`: typed metadata selects users, remove self, dedupe, no `D…` or MPDM ID in channel_ids, paginated completeness, 20/21 boundary produces Eligible/Unavailable with the same authorized target; policy/private-channel deny produces an error before any outward write. Task 3 proves exact-file fallback for 21 members. Task 3 will test the full share-before-grant order.
- [ ] Write `upload v2 uses byte length and parent`: ticket form fields, multibyte byte count, returned URL POST exact original bytes, complete JSON uses one channel, original parent, summary comment. `unsafe upload host never receives token` asserts HTTPS/host checks, egress denial, no redirects/bearer forwarding/logged signed URL. `api failures are sanitized` covers 429, missing_scope, disabled/free-team create, malformed response and content detail containing a secret.
- [ ] Run red; implement adapters and metadata resolution. Never use create(channel_id) as a free-plan recovery path. Never treat the requirements' paid-plan inference as verified.
- [ ] Run green and commit explicit paths.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): add scoped canvas and upload-v2 adapters`

## Task 3: durable artifact delivery and fallback

**Files**

- Create: `src/slack_reply_store.ml`, `src/slack_reply_delivery.ml`, `test/test_slack_reply_delivery.ml` (suite `slack_reply_delivery`).
- Modify: `src/dune`, `test/dune`, `test/test_main.ml`; add recovery cases to `test/test_slack_canvas.ml` and `test/test_slack_file_upload.ml` where they own adapter behaviour.

**Interfaces — consumes Task 1 policy/context and Task 2 adapters; produces persisted delivery outcome**

```ocaml
(* Slack_reply_store; row is abstract, persisted with original body and all stages *)
type row
val init : Sqlite3.db -> unit
(* All keys come from verified installation resolution, never just a legacy session key. *)
val activate_request : db:Sqlite3.db -> key:Slack_reply_types.journal_key ->
  epoch:int -> target:Slack_reply_types.target ->
  (Slack_reply_types.operation_id, Slack_reply_types.error) result
(* Persisted before execution; no final body or policy choice is required yet. *)
val ensure : db:Sqlite3.db -> key:Slack_reply_types.journal_key ->
  epoch:int -> target:Slack_reply_types.target -> policy:Slack_config_types.long_reply ->
  title:string -> body:string -> (row, Slack_reply_types.error) result
val lookup : db:Sqlite3.db -> key:Slack_reply_types.journal_key -> row option
val claim_canvas : db:Sqlite3.db -> operation:Slack_reply_types.operation_id ->
  key:Slack_reply_types.journal_key -> epoch:int ->
  (bool, Slack_reply_types.error) result
val save_canvas : db:Sqlite3.db -> row -> epoch:int -> canvas_id:string -> unit
val save_carrier : db:Sqlite3.db -> row -> epoch:int -> ts:string -> unit
val body : row -> string
val outcome : row -> Slack_reply_types.outcome option
(* Discovery uses installation + session, then returns complete original keys. *)
val recoverable_keys : db:Sqlite3.db -> installation:Slack_reply_types.installation ->
  session_key:string -> Slack_reply_types.journal_key list
(* Core Session_reply_lifecycle owns synchronous tombstoning and epoch checks. *)

(* Slack_reply_delivery *)
type deps = {
  call : Slack_canvas.call;
  upload : Slack_file_upload.transport;
  post : target:Slack_reply_types.target -> text:string ->
    (string, Slack_reply_types.error) result Lwt.t;
  edit : target:Slack_reply_types.target -> ts:string -> text:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
  delete : target:Slack_reply_types.target -> ts:string ->
    (unit, Slack_reply_types.error) result Lwt.t;
}
val deliver : manager:Session.t -> deps:deps ->
  context:Slack_reply_types.context -> body:string ->
  (Slack_reply_types.outcome, Slack_reply_types.error) result Lwt.t
val create_explicit : manager:Session.t -> deps:deps ->
  context:Slack_reply_types.context -> invocation:Slack_reply_context.invocation ->
  title:string -> markdown:string ->
  (Slack_reply_types.artifact, Slack_reply_types.error) result Lwt.t
val resume : manager:Session.t -> deps:deps -> key:Slack_reply_types.journal_key ->
  (Slack_reply_types.outcome option, Slack_reply_types.error) result Lwt.t
```

Store internal methods persist active request/context/epoch before model/tool entry, operation ID, provider invocation ID + replay sequence + original tool arguments before tool execution, then immutable receipt/error before returning to the model, permalink, ticket/stage, policy and safe errors using CAS. `ensure`, `lookup` and request/operation claims all use the same typed verified journal key (team/app/session/request/Final ordinal); ensure rejects mismatched key/target. Recovery enumerates these full keys by verified installation/session; changed installation never reuses another installation's rows. No text/digest-only lookup is allowed.

A request's explicit and automatic final share **one durable operation ID and create fence**. Allocate it before the first explicit invocation or automatic create, not from regenerated title/body. `claim_canvas` is atomic/one-way and epoch-checked; false never authorizes another create. Tool replays consume the persisted invocation sequence and original arguments/receipt/error; altered arguments cannot allocate another operation. Repeated/additional explicit calls for this final reuse compatible success or return already-attempted errors. Any explicit attempt, including failure/unknown success, forbids another automatic create: reuse a known compatible canvas, otherwise file/text with the final's own immutable body. Different content alone is never a new final ordinal. Freeze separate logical final ordinals before their model executions.

`resume` returns None only when no saved final body exists for that full key; otherwise complete/reuse it or return a retained error. A tool-only active request can resume model execution under its saved identity and receipts with its fence intact. Outcome errors propagate through Session_final_reply.Retained_delivery, never sent markers or generic error-reply handlers. The explicit method shares title/link and grants read, but never silently returns a file receipt. Invalid UTF-8 skips both artifacts and uses exact original bytes in plain delivery, retaining failure if rejected.

The additive version-1 schema uses `slack_reply_requests`, `slack_reply_operations`, `slack_reply_tool_invocations` and `slack_reply_deliveries`; Task 1 owns `session_reply_epochs`. It must satisfy Task 1's core Session_reply_lifecycle tombstone contract. Explicit tool bodies/receipts stay in invocation/operation records; do not call final-row ensure with a tool document. Only a completed final initializes that immutable body. Capture epoch before generation, check it on every write/HTTP stage, purge retained content/arguments/receipts atomically on clear while retaining operation fences and owned cleanup IDs. Missing journal tables are valid for minimal/older DBs; SQL failures propagate instead of acknowledging a successful clear. No network operation belongs in the core helper.

**Test-first steps**

- [ ] Register the suite. Write `canvas success posts summary link once`: exact order create → files.info → post → access-set; one visible carrier, read access, no full text posted. `grant failure is not success`: file fallback, original bytes, summary comment, carrier cleanup; failed delete triggers safe edit. `failed canvas cleanup is scoped` asserts only this row's known new canvas is deleted, retained body survives, failed deletion leaves a cleanup flag and never blocks fallback.
- [ ] Write `canvas failure uploads markdown`, `both artifacts fail posts full text`, `file and off modes skip canvas`, `unsupported markdown goes to exact file`, `blank or suppressed reply sends nothing`. Assert exact file body/plain body and no raw API error text.
- [ ] Write `lost create response never creates twice`: crash at each fence/response/persist boundary, reopen DB, retry with changed input text and attempt number; one create total and persisted original answer used. Also `concurrent workers share one canvas`, `known canvas resumes grant`, `explicit duplicate reuses receipt`, `identical explicit writeup reuses final link`, `different final after explicit attempt uses exact file`. Add `lost explicit response blocks automatic create` (explicit timeout then automatic same write-up: one create total, exact file fallback) and `tool replay changed arguments keeps fence` (restart after tool-only attempt; replay consumes original invocation/arguments/receipt, changed generated arguments cause zero new creates).
- [ ] Write `journal lookup is installation scoped`: identical legacy session/request/slot under two teams and two apps never reuse another body/receipt; unresolved/changed installation cannot select either accidentally. `21 member mpdm uploads exact file without canvas` asserts zero create/access calls, exact original file bytes/parent/comment. `policy denial makes no outward writes` asserts zero create/upload/post/grant, with retained body.
- [ ] Write `membership change before grant never shares stale recipients`: mutate DM/MPDM membership after carrier post, before grant; zero access-set calls for the stale list, fallback only after fresh destination authorization. `invalid utf8 preserves original without artifacts`: malformed byte sequence, zero canvas/upload calls, byte-identical plain send; rejected send stays pending with original bytes.
- [ ] Write `ambiguous upload completion is not completed twice` (known file shares reconcile; no new ticket when uncertainty forces full text), `carrier failure retains delivery`, `truncated text is not sent`, `policy revocation stops fallback` (no upload/post after revocation), `cancel does not start fallback`, `journal unavailable never creates canvas`, `reset retains fence but removes answer`. Use temporary DBs and fake faults, no real sleeps.
- [ ] Run red, implement additive schema/transactions and coordinator; keep HTTP outside transactions. Persist body and stage intent before writes. Retain stage state on errors and expose only sanitized failure codes.
- [ ] Run green. Review that private/access/egress errors do not trigger a less-restricted fallback, and commit explicit files.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_delivery
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
```

**Commit:** `feat(slack): journal long-reply delivery and lossless fallback`

## Task 4: wire final paths, buffered streaming and durable replay

**Files**

- Create: `src/slack_message_transport.ml`, `test/test_slack_reply_paths.ml` (suite `slack_reply_paths`).
- Modify: `src/slack.ml`, `src/slack_thread_execution.ml`, `src/slack_thread_runs.ml` only where needed to preserve finish ordering, `src/slack_thread_route.ml`, `src/daemon_inbound_replay.ml`, `src/daemon.ml` (startup ordering), `src/daemon_startup.ml`, `src/daemon_resume.ml`, `src/daemon_util.ml` (background completion finals), `src/connector_dispatch.ml` (idle and queued followup contracts), `src/session_agents.ml` (separate completed final), `src/session_types.ml`, `src/session_core.ml`, `src/session_turn.ml`, `src/session_queue.ml`, `src/session_autonomous.ml`, `src/session_management.ml`, `src/memory_core.ml` (actual clear implementation), `src/dune`, `test/dune`, `test/test_main.ml`.
- Modify every affected queued_message constructor, turn/enqueue caller and Connector_dispatch.dispatch_env constructor explicitly, including `src/discord.ml`, `src/telegram.ml` and other callers found by compiler/search: non-Slack final callbacks use their existing sender, notices unchanged.
- Test: existing `test/test_slack_empty_reply.ml`, `test/test_slack_relay_guard.ml`, `test/test_slack_threads.ml`; locate replay's current suite in `test/test_main.ml` and add regression there if fixture reuse makes it smaller.

**Interfaces — consumes final callbacks and durable request identity; produces one chosen delivery before sent/queue deletion**

```ocaml
(* Slack_message_transport: extracted top-of-handle_message wrappers *)
type t = {
  post : text:string -> unit Lwt.t;
  post_with_id : text:string -> string Lwt.t;
  edit : ts:string -> text:string -> unit Lwt.t;
  delete : ts:string -> unit Lwt.t;
}
val make : manager:Session.t -> route:Slack_thread_route.t option ->
  channel_id:string -> thread_ts:string option ->
  legacy_send:(bot_token:string -> channel_id:string -> text:string -> unit Lwt.t) -> t

(* Slack_reply_delivery additions *)
val install : Session.t -> unit
val send_final : manager:Session.t -> context:Slack_reply_types.context ->
  fallback:(string -> unit Lwt.t) -> string -> unit Lwt.t
type recovery = No_active_request | Needs_model of Slack_reply_types.journal_key
  | Recovered | Retained of Slack_reply_types.error
val recover_session : manager:Session.t ->
  installation:Slack_reply_types.installation -> session_key:string -> recovery Lwt.t
val recover_installation : manager:Session.t ->
  installation:Slack_reply_types.installation -> unit Lwt.t
val resume_before_model : manager:Session.t ->
  key:Slack_reply_types.journal_key -> recovery Lwt.t

(* Connector_dispatch: notices/acks stay on send_reply; only completed finals use send_final. *)
(* dispatch_env adds final_context : Slack_reply_types.context option;
   send_final : Slack_reply_types.context -> string -> unit Lwt.t *)
(* dispatch_followup adds ?delivery_context and ~send_final; queued_followup preserves context. *)
(* Session_agents.agent_invoke_turn/delegate_turn/fork_and_run add
   ?delivery_context and ~send_final, retain ~send_reply and ?debug_notify. *)
(* Session.turn/turn_stream/turn_verified_stream/turn_synthetic add
   ?delivery_context : Slack_reply_types.context; enqueue retains it unchanged. *)
```

`install` assigns the neutral callback; it never wraps the progress registry. `send_final` returns only on confirmed success/suppression; safe errors raise Retained_delivery. Coordinator post/edit adapters must decode Slack `ok`, errors and truncation warnings: current unit-returning helpers cannot prove successful delivery. Explicitly catch retained-delivery failures before generic catches in Slack.handle_message (including before_drain), Connector_dispatch, Session_agents, autonomous, Daemon_util and Daemon_resume. Finish status/cleanup, but no generic error post, sent marker or model rerun. Preserve the body and schedule bounded backoff journal retries, also recovered at startup; policy denial/cancellation forbids retries' outward writes.

**Durable context codec and propagation (owned here):** add `delivery_context : Slack_reply_types.context option` to Session_types.queued_message and all constructors. Encode `delivery_version=1` and a `delivery_context` object with connector, canonical session, channel, explicit nullable thread_ts, requester, stable request_id and ordinal; leave `origin_version=1`/execution_origin/synthetic_owner checks intact. Extend replay_payload/decoder for both native and legacy routes, and persist upgraded rows before running them. Context is provenance for routing, not authorization. Resolve verified team/app at request activation and store it with the active request, independent of the queue.

At ingress create context from original Slack ts/channel/parent, not from the latest send closure. Supply it through all turn/stream/synthetic/enqueue APIs and Connector_dispatch followup queue/append paths. Append-to-existing-followup preserves that queued request's ID/parent; it cannot borrow the appending turn's context. For old rows use trusted message_id, otherwise `queue:<queue_id>`; native root comes from verified route, missing legacy parent defaults to historical root None. Missing synthetic ID is durably allocated/upgraded once (queue ID for old queued work); new synthetic IDs are persisted before scheduling. Unknown/malformed delivery versions are retained with no guessed target/tool authority. Never serialize tokens or treat a serialized installation as proof of identity. Capture epoch before model entry and reject writes from a cancelled epoch.

**Startup and live recovery:** install lifecycle schema/delivery after DB/config setup; in Daemon.ml invoke recover_installation for each verified installation **before starting connector intake, run_pending_session_resume_stage and run_durable_replay_stage**. Under per-session locks reauthorize and inspect active records, including confirmed-but-not-marked finals; reconcile sent state without model execution. Recover_session is also the first guard in Daemon_resume.resume_agent_session, ahead of its notice/direct Agent.turn; pending-session enumeration defers any retained journal request even without an inbound row and regardless of its age filter. Recheck immediately before direct model entry to cover races. No_active_request allows existing resume; Needs_model restores the original identity/epoch and tool ledger; Recovered returns without a model; Retained blocks model entry and leaves retry state. Replayed/queued turns use resume_before_model with the same complete key. Discovery comes from the journal, never reconstruction of a request ID from a session key. Confirmed outcomes plus active-request/sent state remain until reconciliation, not queue deletion.

**Final call sites:** followup's idle before_drain and result branches call the final sender before mark_response_sent; queued followups retain context for Session drain. Session_agents' only final call is the completed Agent.turn response in each of invoke/delegate/fork; document notices, missing-template/draining/errors and debug output use ordinary send_reply. Background completion uses persisted `background:<task-id>:completion` context and completion_owner before turn_synthetic; its completed final calls the durable sender before marking sent and before continuation dispatch. A queued suppression transfers ownership to drain without marking. Autonomous/resume final producers likewise allocate their logical request/ordinal before model entry. Generic notifiers remain unchanged.

**Clear/reset:** call Session_reply_lifecycle.clear_session synchronously at the start of Memory_core.clear_session; Session_management.reset uses this in both phases, with cancellation/epoch invalidation before waiting for in-flight work. Tombstoning is network-free, core-owned SQL; absent journal tables are supported. Failure propagates as clear/reset failure and blocks affected old deliveries. Each callback/save checks epoch; already-sent network calls can only record owned cleanup/fence when they return. Test the public Memory.clear_session and Session.reset, not only a store helper.

**Test-first steps**

- [ ] Register `slack_reply_paths`. Write `native long final never streams body`: fake provider emits >2500 characters in multiple deltas; start/maintenance/stop contain status only, no append of answer, one summary/link post. Exercise native stream unavailable and long maintenance rotation too.
- [ ] Write `posted root thread dm and mpdm preserve target`: exact channel/parent matrix, metadata-driven sharing, no reply_broadcast, file fallback same destination. `drain uses queued request identity`: queue A/B with different humans; final B never uses A's IDs/authority. Test response_sent and before_drain produce only one delivery.
- [ ] Write `idle and queued followup use overflow`: exercise Connector_dispatch.dispatch_followup in idle and busy modes, long answers, legacy threaded and native destinations; both idle final branches and queued drain produce one artifact with original request/parent, acknowledgements/progress produce none. `agent document notice cannot claim final slot` runs each Session_agents entry point with a threshold below the document-load notice length and then a long completed answer: no artifact/final journal row for notice, complete final delivered once; validation/debug/error receipts also stay ordinary.
- [ ] Write `live journal without inbound row resumes before model`: immediate live Slack reply, assert no inbound_queue row exists, then restart at (a) all sends failed and (b) successful canvas/grant before sent marker. Startup and direct Daemon_resume entry both discover original context/body; zero extra model/create calls, pending state in (a), reconciled sent in (b). Retry (a) using saved body when fake transport recovers. Exercise retained failure through Slack.handle_message to prove no generic error post/mark-sent.
- [ ] Write `background completion final uses overflow delivery` and `background completion failure retries saved answer`: invoke the real Daemon_util.inject_background_task_completion with task ID, route and owner; immediate/queued cases share stable identity and original parent, one canvas, mark only after success. Inject all-stage failure then retry/restart: no sent marker, no extra model/create calls, original answer recovered.
- [ ] Write `replay resumes stored final before model`: generate/create once, crash before queue-delete, reopen DB/replay; zero second model invocation and zero second create. Also replay without original message ID uses stable queue ID; changed attempt count does not change key. `synthetic continuation id survives retry` covers autonomous/agent-command final slots. `queued delivery context survives codec restart` round-trips two legacy parents plus root null and a persisted synthetic ID, reopens DB and drains through another active closure; original IDs/parents survive exactly. Legacy versions receive the stated defaults; malformed versions never gain authority.
- [ ] Write `relay and notices bypass overflow`: long standing-order post, tool progress, thinking, pause receipt and card cause no artifact; long actual final does. `reload during generation changes delivery` and `reload after claim keeps chosen artifact` assert mode/threshold timing and refreshed authorization. `clear prevents pending answer resurrection` invokes both Memory.clear_session and Session.reset, including reset while a fake create response is in flight: content/arguments/receipts erased, fence retained, no later grant/upload/post or stale body write. Include absent-table and SQLite clear-failure cases; a failed clear is never reported successful. A transmitted create may return only an owned cleanup ID.
- [ ] Run red. Extract transport wrappers and rewire only actual finals: live before_drain/result, completed Session_agents Agent.turn responses (split send_final from send_reply/debug_notify), Connector_dispatch idle followup before_drain/result, queue-final emission in Session, autonomous on_response, native/legacy replay before_drain, Daemon_util background completion and Daemon_resume completed final sends. Keep `Slack_thread_execution.notify` and registered progress string callbacks unchanged. Use Slack_reply_context.with_context scope around model execution for Task 5 prompt metadata.
- [ ] Implement the explicit startup ordering and Daemon_resume guards above, plus journal retries independent of inbound_queue. Preserve native preparation/authorization even when reusing a delivered result; suppress duplicates without bypassing current policy. Verify live records and confirmed-but-unmarked outcomes are discovered before any model resume.
- [ ] Ensure `slack.ml` shrinks below 1121 lines. Inventory every final callback and every generic notify use; retain exclusions. Run green, full/optimized validation for runtime reshaping, then commit explicit files.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_empty_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_relay_guard
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune exec -j 2 test/test_main.exe -- test'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune build -j 2 --profile release-speed --build-dir _build_opt_speed src/main.exe'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): route final answers through durable canvas delivery`

## Task 5: explicit tool, contextual guidance and templates

**Files**

- Create: `src/slack_canvas_tool.ml`, `test/test_slack_canvas_tool.ml` (suite `slack_canvas_tool`).
- Modify: `src/tools_builtin.ml` (core placeholder/schema), `src/daemon_startup.ml`, `src/daemon_util.ml` only for additional tool reload rebinding (background finals are already owned by Task 4), `src/slack_huddle_context.ml` (DM-compatible validated resolution), `src/room_tool_availability.ml`, `src/agent_profile.ml`, `src/prompt_builder.ml`, `src/agent_2_tools.ml` (tool invocation provenance), `src/slack_reply_context.ml` (scoped invocation metadata), `src/agent_1_context.ml`, `src/agent_turn_setup.ml`, `src/session_turn.ml` for verified metadata propagation, `src/dune`, `test/dune`, `test/test_main.ml`.
- Modify: `docs/builtin-agent-prompts/researcher.md`, `docs/builtin-agent-prompts/documenter.md`, generated `src/agent_template_builtins_research.ml` and any other generated group actually changed by regeneration. Test: `test/test_prompt_builder.ml` and B844's existing availability suite.

**Interfaces — consumes current Slack authority and journal; produces schema-compatible tool and per-request prompt section**

```ocaml
(* Tools_builtin: core placeholder, exact schema/description from spec *)
val slack_canvas_create : unit -> Tool.t
(* Slack_canvas_tool: full implementation replaces placeholder *)
val make : current_config:(unit -> Runtime_config.t) -> Tool.t
val register : current_config:(unit -> Runtime_config.t) -> Tool_registry.t -> unit
(* Prompt_builder: reads neutral turn context, no integration import *)
val slack_reply_guidance : connector:string option ->
  config:Runtime_config.t -> tool_available:bool -> string option
(* Room_tool_availability addition, composed with existing checks *)
val slack_canvas_available : connector:string option -> bool
```

The pure `slack_reply_guidance` helper feeds `build` using `Slack_reply_context.current_context ()`; this avoids changing every `build` caller and keeps guidance tied to verified execution, including rebuild after compaction. `Agent_profile` uses the same neutral connector metadata to hide the tool outside Slack; the invocation guard still uses Slack_huddle_context plus access snapshot. Add placeholder to core registration; full startup replaces via `Tool_registry.replace` (not duplicate register). Respect tools_enabled and template filters; current config access must survive reload. Task 1's low-level context is presentation metadata, never authority. Check the completed module graph: Slack_reply_context → Slack_reply_types/Lwt only; prompt/Agent_profile → Slack_reply_context; Session_final_reply → Session_types → Agent remains separate. The minimal gate must compile this completed graph after all prompt changes.

`Agent_2_tools.execute_tools` already sees `Provider.tool_call.id` but the current `Tool.invoke_context` does not expose it (`src/agent_2_tools.ml:204,366–384,460–516`; `src/tool.ml:3–18`). Before parallel invocation, assign cumulative request-local sequence positions in provider call order and scope each actual invoke with `Slack_reply_context.with_invocation {provider_call_id=tc.id; sequence}`. Initialize/restore the sequence cursor at the persisted model replay boundary, not from Lwt completion order or regenerated IDs. The canvas integration consumes that metadata plus the active key, writes original effective arguments before its first remote operation, and returns the recorded immutable result/error for a replayed position even when the provider generates a new ID or arguments. An invocation that crashed without a receipt resumes from known stages or returns a safe unknown-outcome error with the fence still consumed. No integration import enters Agent_2_tools; no new tool argument lets the model choose identity. Add `tool invocation metadata survives parallel and replay ordering` to the tool suite, asserting deterministic positions, original arguments/receipts and zero second create after changed regenerated calls. Missing metadata fails with an actionable error before any network call.

**Test-first steps**

- [ ] Register `slack_canvas_tool`; write `outside slack gives actionable error` (exact repair guidance, zero network), `arguments cannot select destination` (unknown fields/token/channel rejected), `valid slack tool shares read only` (title/link carrier, read grant, returned JSON only after success), `repeated call reuses artifact` (same durable invocation/operation, original persisted arguments, one create even if replayed arguments differ), `failed explicit canvas returns error not fake link`.
- [ ] Write `canvas title validation rejects blank invalid and oversized titles without network`: trim valid title, accept 120 Unicode scalars even if multibyte; reject blank/whitespace, malformed UTF-8 and 121 scalars, zero network/attempt claims for rejected arguments. Schema length is not a replacement for runtime scalar/UTF-8 validation.
- [ ] Write `dm tool resolves verified recipients` without widening existing standing-order/huddle grants. Authorization refactor must keep their previous restrictions intact. `stale or injected context cannot publish` proves snapshot/context invalidation remains effective.
- [ ] Add prompt cases `slack canvas guidance includes threshold`, `file and off guidance match behaviour`, `non slack omits guidance`, `hidden tool omits advice`, `static prompt still includes delivery guidance`, `compaction and reload refresh guidance`. Use neutral context scoped around each build; prevent Slack guidance leaking between tests/turns.
- [ ] Add `minimal tool is disabled` at the core placeholder; assert exact schema/description contract and B844 filtering. Run red; implement and replace at startup. Apply the spec's exact description/prompt strings, with mode interpolation only.
- [ ] Review every built-in prompt's allow/disallow lists. Grant only researcher/documenter by default; preserve existing restrictions. Generate built-ins through gate, inspect explicit changed paths, verify generated output and minimal build, then commit.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas_tool
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite prompt_builder
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
```

**Commit:** `feat(slack): expose canvas tool and contextual reply guidance`

## Task 6: operator documentation and final acceptance

**Files**

- Modify: `docs/src/content/docs/channels.mdx`, `docs/setup-guide.md`, `docs/src/content/docs/configuration.mdx`, `docs/public/llms-full.txt`.
- Test: extend `test/test_slack_reply_paths.ml`, `test/test_slack_canvas_tool.ml` only if acceptance exposes missing behavioural coverage; no new suite, and no implementation-mirroring documentation tests.

**Interfaces — consumes final public configuration/tool contract; produces operator instructions**

No new OCaml API. Document these existing produced signatures and their operational contract:

```ocaml
Slack_config_types.long_reply_of_json : Yojson.Safe.t -> Slack_config_types.long_reply
Slack_reply_delivery.deliver : manager:Session.t -> deps:Slack_reply_delivery.deps ->
  context:Slack_reply_types.context -> body:string ->
  (Slack_reply_types.outcome, Slack_reply_types.error) result Lwt.t
Slack_canvas_tool.make : current_config:(unit -> Runtime_config.t) -> Tool.t
```

**Test-first/acceptance steps**

- [ ] Before docs, run every new suite from Tasks 1–5, including named mention/broadcast/config-loader/title cases, membership race, malformed UTF-8, installation isolation, explicit-to-automatic fence faults, idle/queued followup, agent notice, live-no-queue recovery, background completion and public clear/reset race tests. Run Task 4's `native long final never streams body`, `replay resumes stored final before model`, `relay and notices bypass overflow`, and Task 5's `outside slack gives actionable error` suites. Add a failing case first if the final behaviour differs from the approved spec; fix it in the owning task's module and report the deviation.
- [ ] Document defaults/modes, threshold count, opening-block rule, runtime config-set examples, file fallback, off-vs-explicit-tool distinction, private/DM/MPDM scope and limits, static prompt guidance, retained failure/journal behaviour and no-public-sharing policy. Use the same public API citations as the spec.
- [ ] Setup: list `canvases:write`, `files:write`, **`files:read`**, existing `chat:write`, `users:read` for installation/identity checks, and conversation read scopes; explain reinstall after OAuth scope additions. `canvases:read` is not required by this create/permalink flow. Document Slack API POST/GET and upload-host egress separately, no broad wildcard credential forwarding. Explain free-team fallback and that native canvas unfurl layout is not guaranteed.
- [ ] Record a coordinator-only live acceptance checklist: long root/thread/private/DM/MPDM, read-only recipients, summary/link rendering, share-before-grant behaviour, then file fallback. State that Max/coordinator does this after deployment; never execute it with tokens during implementation. Carry both pending-Max defaults into the handoff.
- [ ] Run all acceptance commands below sequentially. Confirm every new suite appears in the test list, generated agents are clean, minimal links without integrations, new files <1000 lines and `slack.ml` <1121. Check explicit diff for extraction/generated drift and secrets. If all checks pass, commit only the four docs (and explicitly identified regression files if changed); report exact commands/results and deliberate limits. No deployment/push/merge/rebase.

**Verification:**

```bash
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh raw 'dune exec -j 2 test/test_main.exe -- list'
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_long_reply
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_loader
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite config_set
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_file_upload
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_delivery
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite prompt_builder
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_reply_paths
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh suite slack_canvas_tool
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh quick
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh fmt-check
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh minimal
/home/xertrov/src/clawq/.superpowers/sdd/huddle-orders/bin/gate-canvas.sh gen-agents-check
```

**Commit:** `docs(slack): document canvas replies scopes and fallback`

## Design review closeout

All twelve Important findings are incorporated in the owning tasks and the spec's review responses. No finding is rejected. The implementation must preserve the revised contracts as a whole; in particular, tool and automatic creation may not use independent digest/final fences, and pending-session resume may not precede journal recovery. This design revision does not execute the implementation gates or change source code.

Questions requiring Max's judgment remain the spec's two explicit defaults: DM/MPDM grants surviving departure, and accepting share-before-read publication subject to coordinator verification. API verification belongs to the coordinator; failed grant verification falls back without additional direct messages or broadened permissions.
