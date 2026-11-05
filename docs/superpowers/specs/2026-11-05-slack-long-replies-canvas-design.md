# I065: long Slack replies in canvases

Design date: 2026-11-05. Binding input: `.superpowers/sdd/2026-11-05-slack-canvas-replies/requirements.md`. This is a design, not an implementation or deployment receipt.

## Intent, goals and boundaries

Keep Clawq's final Slack answers readable in conversation: above 2500 characters, put the complete answer in a read-only canvas and show its opening paragraph plus a link. An explicit research tool offers the same destination without waiting for overflow. Preserve the complete answer through canvas → Markdown file → ordinary message fallback. Apply current room policy on every operation, and create at most one canvas per logical reply across retries and durable replay.

No HTML hosting, public links, org-wide sharing, standing-order changes, canvas editing tool, general file-upload tool, or answer summarisation by a second model. Relay posts, progress, thinking, receipts, notices and cards retain their current handling. This work does not enable private channels or broaden a room's policy.

## Findings from this checkout

- `Slack.handle_message` in `src/slack.ml` builds a generic `reply` closure used for both notices and final output. Final output occurs in `before_drain`, the non-drain result branch, agent-command callbacks and autonomous `on_response` callbacks. Hook these final call sites; wrapping `Slack_api.send_message` would wrongly affect notices and relays.
- `Slack_thread_runs.open_stream` sends only `Working…`. Maintenance rotates that status stream; `finish` stops it. There is **no `chat.appendStream` call** in this checkout. `Status_update.make_handler` ignores answer `Provider.Delta` events. `Session.turn_verified_stream` means streaming provider events, not streaming answer text to Slack. Final answer text is posted separately after finalisation.
- `Slack_thread_execution.notify` is a plain threaded post. Its registered notifier is also used by progress. Durable replay's `native_replay_turn` calls it from `before_drain`; changing this generic function into an overflow sender would blur final/progress boundaries.
- `Slack_thread_route` encodes team/app/channel/root in the native session key. Legacy keys can identify only a channel or user-specific session. Do not infer DM kind or sharing membership from a `C/G/D` prefix: use typed conversation metadata (`is_im`, `is_mpim`, `is_private`).
- `Slack_thread_api` supplies current-config egress/allowlist checks and native authorization. Canvas and upload calls must preserve these checks, including the separate upload-host request. `Slack_huddle_context.clear` runs before final sending: final delivery must carry explicit server-produced provenance rather than consult that cleared cell.
- `daemon_inbound_replay.ml` reclaims stale/failed rows, runs an at-least-once turn and deletes the row after success. Original Slack message provenance is present only on appropriately versioned live rows. A response delivery journal must survive deletion/reclamation and must be consulted before rerunning a model.
- Contrary to the requirements' expectation, a search of `src/` and `test/` for `getUploadURLExternal`, `completeUploadExternal`, `initial_comment` and upload-v2 spellings finds **no Slack upload-v2 implementation** on this branch. `Teams_file_consent` is unrelated. Implement a small Slack upload-v2 adapter; do not assume a missing helper or copy a standing-order plan's future code.
- Config is split across `Slack_config_types`, `Runtime_config_types.slack_config`, `Config_loader_channels`, `Runtime_config_json_sections` and `Config_set`. Runtime reads must use `Session.get_config`, not the ingress closure's config. `Daemon_config_reload` already refreshes manager/active agents and registries.
- `Tool_registry` serializes tool descriptions and strict object schemas; `Room_tool_availability` hides unusable room tools (B844). `Prompt_builder.build` has `channel_type` but no verified connector argument; Slack guidance must receive explicit turn metadata. The `[Context: …]` block in `Session_context` names the room, not necessarily its connector.

## Slack API contract, with sources

The following facts were checked against public docs on the design date; no authenticated Slack request was made.

| Fact | Source and design consequence |
| --- | --- |
| `canvases.create` accepts optional `title`, `document_content = {type:"markdown", markdown:…}`, and `channel_id`; returns `canvas_id`, not a permalink. Bot scope is `canvases:write`. | [canvases.create](https://docs.slack.dev/reference/methods/canvases.create/). Create a standalone canvas, omitting `channel_id`. |
| Supplying `channel_id` adds a canvas tab **with write permissions**. Free teams require that parameter, allow one canvas tab per channel, and can return `free_teams_cannot_create_standalone_canvases` or `free_team_canvas_tab_already_exists`. | [canvases.create](https://docs.slack.dev/reference/methods/canvases.create/). Never retry with `channel_id`; fall back to a file. Amaroo's paid-plan status is not established by this design run. |
| Content limit is documented as “1 MiB (1,048,576 characters)”. Markdown supports tables, code blocks/spans, headings h1–h3, lists, links and mentions; Block Kit is unsupported. Tables have at most 300 cells. | [Canvas surface](https://docs.slack.dev/surfaces/canvases/), [create](https://docs.slack.dev/reference/methods/canvases.create/). Enforce both 1,048,576 UTF-8 bytes and code points conservatively after preparation; unsupported content falls back losslessly. |
| Canvas-specific mentions use `![](@U…)` and `![](#C…)`; canvas links can appear as `https://<workspace>.slack.com/docs/<team>/<canvas>`. | [Canvas surface](https://docs.slack.dev/surfaces/canvases/). Do not feed Slack message `mrkdwn` through a blanket replacement pass. The documented URL examples are not a guarantee for a fabricated permalink. |
| `canvases.access.set` requires `canvas_id` and `access_level`; exactly one of `channel_ids` or `user_ids`, each 1–20 entries. `read` grants read access. It requires `canvases:write`. Share the canvas link in the target first, then change its access at that same channel/user level. | [canvases.access.set](https://docs.slack.dev/reference/methods/canvases.access.set/). Delivery is create → permalink → share message → set read access → acknowledge success. |
| Access setters reject DM and MPDM conversation IDs; individual `user_ids` must be used instead. The access API says canvases require a paid plan. | [DM/MPDM subsection](https://docs.slack.dev/reference/methods/canvases.access.set/#sharing-canvases-via-dm-or-mpdm). Use only members of the originating DM/MPDM, never open separate DMs to grant access. |
| `files.info(file=canvas_id)` requires `files:read`; file objects expose `permalink`, separately from `permalink_public`. | [files.info](https://docs.slack.dev/reference/methods/files.info/), [file object](https://docs.slack.dev/reference/objects/file-object/). Require `files:read` for the selected permalink flow; never use `permalink_public`. `canvases:read` is not required by create/access-set/files-info. |
| `chat.postMessage` accepts conversation IDs and optional parent `thread_ts`; bot scope is `chat:write`. Links unfurl by default, but docs do not promise the layout/content of a canvas preview. | [chat.postMessage](https://docs.slack.dev/reference/methods/chat.postMessage/), [unfurling](https://docs.slack.dev/messaging/unfurling-links-in-messages/). A labelled canvas link is sufficient; no `chat.unfurl`, `links:read` or `links:write` dependency. Suppress media/link unfurls to keep the summary compact. |
| `files.getUploadURLExternal` is form-encoded, takes `filename` and byte `length`, returns upload URL/file ID. POST raw bytes or multipart to that URL; HTTP 200 means success. | [getUploadURLExternal](https://docs.slack.dev/reference/methods/files.getUploadURLExternal/). This is a separate HTTP operation, not a JSON Slack method call. Both upload API methods require `files:write`. |
| `files.completeUploadExternal` takes `files`, `channel_id`, optional parent `thread_ts`, and `initial_comment`; omission of channel leaves the file private. Complete once; without completion Slack discards the upload. | [completeUploadExternal](https://docs.slack.dev/reference/methods/files.completeUploadExternal/). Use exactly one originating conversation and its original parent, with summary as comment. Never fan out with `channels`. |
| `conversations.info` distinguishes IM/MPIM/private metadata; `conversations.members` is cursor-paginated. Their bot read scopes depend on kind: `channels:read`, `groups:read`, `im:read`, `mpim:read`. | [info](https://docs.slack.dev/reference/methods/conversations.info/), [members](https://docs.slack.dev/reference/methods/conversations.members/). Missing metadata or incomplete membership is not permission to share. |
| `auth.test` returns team ID and workspace URL with no added scope. | [auth.test](https://docs.slack.dev/reference/methods/auth.test/). Validate installation/team; do not hard-code Amaroo's URL for all installations. |
| `bots.info` accepts the bot ID, exposes `app_id`, and requires `users:read`. | [bots.info](https://docs.slack.dev/reference/methods/bots.info/). Legacy ingress without a native route/envelope app ID resolves its own bot via `auth.test.bot_id`, then validates that bot/app. Missing installation identity selects ordinary delivery, never a guessed journal namespace. |
| `canvases.delete` accepts `canvas_id`, requires `canvases:write`, and permanently deletes that canvas. | [canvases.delete](https://docs.slack.dev/reference/methods/canvases.delete/). Cleanup may target only the newly created canvas recorded on this delivery row, after the full answer is safely retained. |
| Create errors include `canvas_creation_failed`, `canvas_disabled_user_team`, `missing_scope`, `restricted_action`, `ratelimited`, `internal_error`, `fatal_error`; the last two can occur after partial success. The documented arguments contain no client idempotency key. | [canvases.create errors](https://docs.slack.dev/reference/methods/canvases.create/). A write-ahead local attempt fence, rather than blind retries, enforces at-most-one create. |

Useful automatic canvas unfurling is **not documented as a stable API guarantee**. The summary/link presentation works without it. Likewise the docs do not establish transient permissions between link sharing and access-set, or verify that an MPDM share satisfies the user-level sharing prerequisite. These are coordinator acceptance checks, not reasons to invent an API. On any unsuccessful grant, use fallback. Before enabling canvas mode in deployment, the coordinator must verify that the share-then-read sequence gives the intended read-only result in channel, private channel, DM and MPDM; agents use fake HTTP only.

## Send-path behaviour

The policy is final-only and evaluates `code_points(original_reply) > threshold`; equality stays in chat. Whitespace-only/suppressed replies never create an artifact or send an empty message (B845).

| Path | Final delivery |
| --- | --- |
| Live native channel/private-channel thread | Buffer answer deltas. Finish status stream and progress, then pass original final answer and immutable request ID into `Slack_reply_delivery`. Post the chosen outcome under `route.root`; never broadcast. |
| Live native DM/MPDM thread | Same buffering/finalisation, original conversation/root, user-level canvas access as below. |
| Legacy posted channel/DM or explicit thread reply | Invoke delivery at the final-answer callback, preserving its exact `channel_id` and optional `thread_ts`. Never convert a root response into a thread or create another conversation. |
| Queue drain, restart replay | Use the same delivery function with the queued request's durable identity, not the latest live turn's closure. Reuse stored final answer/artifact before any model rerun. Preserve existing owner authorization and FIFO/retention rules. |
| Autonomous continuation / agent-command final answer | Use a persisted continuation/invocation ID and final ordinal. It is a new logical reply, delivered through the same final-only entry point. Debug/status callbacks remain ordinary sends. |
| Standing-order relay, notices/cards, progress, thinking, pause receipts, error receipts | Bypass overflow entirely; keep original generic send/notifier functions. |

Introduce a neutral core final-delivery callback, separate from existing string progress notifiers. Its context supplies connector, room, optional parent, requester, durable request ID and final ordinal. Session final exits and replay use it; non-Slack falls back to the existing sender. Integrations install the Slack implementation. Explicit `before_drain` and `response_sent` branches must converge on one delivery, with the sent marker set only after delivery returns successfully. A generic notifier can never decide that arbitrary text is a final answer based on length.

Keep status-only native streams in all modes, including `off`. If future code introduces answer `appendStream` calls, it must buffer them until the final policy decision; this feature must not stream full text and later post a canvas. Do not append the summary/link to the existing “Working…” stream and also post it.

## Opening paragraph and artifact content

Count Unicode scalar values, not OCaml bytes or grapheme clusters, using validated UTF-8. Threshold tests include multibyte text. Invalid UTF-8 bypasses canvas preparation and follows the existing full-text failure path; never silently substitute answer bytes.

For summary extraction only, normalize CRLF/CR to LF and trim surrounding whitespace. The opening paragraph is the first non-empty run of lines terminated by an empty or spaces/tabs-only line **outside a fenced code block** (backtick/tilde fences of at least three characters, closed by matching character and sufficient length). Leading headings, lists or code count as part of that opening block; do not skip them in search of prose. Join its line breaks with one space and trim. There is no semantic rewrite.

The summary is at most **600 Unicode scalar values**, including an appended `…`. If the opening block is longer, keep its first 599 scalar values, right-trim whitespace, append `…`. With no paragraph break, use this same capped prefix of the entire reply. If that produces no text, use `Full answer`. These are presentation defaults, not new config fields. Escape literal Slack `<`, `>` and `&` in summary text, with mention expansion disabled; label the separate trusted link `Read full answer`. The 600 limit is measured before Slack escaping. The canvas/file always contains the full original answer, including its opening paragraph.

`Slack_long_reply_policy.prepare_markdown` preserves ordinary Markdown tables (≤300 cells), fenced code, inline code, headings h1–h3 and links. Outside code only, adapt `<@U…>`/`<#C…|label>` to documented canvas mentions, and `<https://…|label>` to Markdown links, preserving displayed meaning. Render broadcast tokens (`<!channel>`, `<!here>`, `<!everyone>`) literally rather than notify a wider audience. Reject unsupported HTML, headings deeper than h3, oversized tables, malformed/ambiguous constructs or a preparation that cannot preserve content; choose the exact `.md` file rather than dropping text. No remote image fetching, external hosting, hidden metadata, added history, transcript or prompt. File bytes and full-text fallback are always original bytes. Automatic title: `Clawq answer` plus the inbound Slack timestamp; explicit tool title is trimmed, non-empty, valid UTF-8, at most 120 scalar values.

## Canvas creation, sharing and access

1. Resolve server-owned target, current config, originating requester and typed conversation metadata. Reauthorize existing allowlist, private-channel policy, room policy, membership, installation and egress rules. Incomplete metadata denies canvas/file sharing. For a DM derive its exact human recipient; for MPDM enumerate all current members with complete pagination (≤10 pages of 200), remove the app's bot user and deduplicate. More than 20 recipients or any unresolved identity selects file fallback; never grant a subset or broaden scope.
2. Persist original final text, summary, target and effective policy. Atomically claim the canvas attempt before calling `canvases.create` once, with no `channel_id`. Persist returned `canvas_id` immediately. On any unknown outcome, do not create again.
3. Read `files.info` and validate its HTTPS `file.permalink` belongs to this installation's Slack workspace; reject a public permalink or mismatched file ID. Failure (including missing `files:read`) selects file fallback, without trying a guessed URL. Reuse known canvas ID on retries.
4. Post **one share carrier** in the original conversation/parent, with summary plus labelled permalink. Save its `ts`. This is the final visible message on success. Then set `read` access: regular/private channels use `channel_ids:[origin]`; DM/MPDM use the exact recipient `user_ids`. No extra direct messages. Recheck config and recipient membership immediately before grant; if it changed since preparation, fall back rather than grant a stale list.
5. Only return success after access-set succeeds. Link sharing and read-setting cannot be atomic. The coordinator must verify the prerequisite/permission behaviour stated above. A failed access-set is a failed canvas delivery, even when creation and sharing succeeded.

The creator/app retains ownership; read-only describes recipients. Canvas content is private to the originating conversation's recipients, subject to Slack's own retention, admin and sharing semantics, just as ordinary messages are. Do not invoke public sharing APIs or manipulate an existing unrelated canvas.

**Default pending Max:** DM/MPDM user grants capture the recipients at send time. Slack's API offers no conversation-bound canvas access for them, so a later departure does not automatically revoke a user grant. Default to this documented user-level flow, with file fallback if the flow cannot establish read access; Max can instead choose file-only automatic delivery in DMs/MPDMs.

**Default pending Max:** Accept Slack's required link-share-before-read sequence, with a brief inaccessible or Slack-default-access interval and automatic fallback when read-setting fails. Default to this API-supported order; if strictly atomic read-only publication is required, use file mode until Slack supplies an appropriate primitive. This decision does not authorize intentionally granting write access.

## Fallback, failures and delivery journal

Canvas mode: canvas → file → full text. File mode: file → full text. Off mode: original full-text send. The explicit canvas tool is still available in all modes: mode controls automatic overflow only.

The file is `clawq-answer-<stable-reply-digest>.md`, UTF-8 Markdown with the original text. Get an upload ticket (byte length), POST bytes to its validated Slack HTTPS upload URL, complete with exactly `channel_id`, optional original root `thread_ts`, and `initial_comment=summary`. Policy-check the ticket, upload host and completion independently; do not forward a bearer token to an arbitrary upload URL, follow redirects, or log that URL. A missing required scope or size/plan limit is an ordinary fallback condition.

If a canvas share carrier already exists and later steps fail, keep its timestamp. On confirmed file success, remove that carrier; if deletion fails, edit it to `Full answer attached below.` without the stale canvas link. On file failure, edit the carrier to the original full answer; if it no longer exists, use the ordinary full-text post. After safely retaining/delivering the body, best-effort delete a known newly created canvas whose access grant failed; retain a cleanup-needed flag if policy or Slack prevents deletion. An explicit tool failure retracts its carrier (or edits it to a short safe failure notice) and performs the same owned-canvas cleanup before returning the actionable error. Artifact cleanup failure never suppresses the answer. If Slack rejects or truncates a full-text answer, retain the durable body and mark delivery failed, not sent: the existing plain-send behaviour cannot guarantee arbitrarily large Slack text. Slack documents a 40,000-character truncation limit in [chat.postMessage](https://docs.slack.dev/reference/methods/chat.postMessage/). Do not silently declare a truncated answer delivered.

`Slack_reply_store` owns an additive SQLite table `slack_reply_deliveries`, independent of `inbound_queue`. Unique key: installation team + app + canonical session + durable request ID + logical reply slot (`final:<ordinal>` or `tool:<content-digest>`). Never key a final reply by response text, random stream reference, process ID or queue attempt count. Live Slack requests use original message `ts`; queued rows preserve it; legacy rows without it use `queue:<queue_id>`. Persist synthetic request IDs before execution. Explicit tool calls deduplicate the same title/body digest within a request; a second distinct explicit canvas is an intentional separate artifact.

The row stores immutable original body/summary/target/config choice, content digest, canvas attempted flag, canvas ID/permalink, share `ts`, upload ticket ID/stage, terminal outcome and safe last error. Changes use transactional compare-and-set; a per-reply Lwt lock serializes local waiters, and SQLite uniqueness/claims protect independent workers. No SQLite transaction spans HTTP.

The create fence is durable **before** network transmission. On timeout, cancellation, process crash, 429, Slack error, malformed success or a crash between success and ID persistence, the fence remains set. Reclaiming a worker lease never clears it. Resume a known ID; otherwise skip create permanently for that reply and proceed to file. This may leave an orphan private canvas after an ambiguous response, but cannot create a second one. Do not search by title to infer ownership or retry create after a “safe-looking” error.

Before model entry during replay, look up this request's persisted final rows. Resume delivery from their stored body or return their confirmed terminal outcome; do not regenerate another answer/canvas. Persist a generated final before the first send; a crash earlier can rerun the model because there was no artifact attempt. Keep tool receipts durable so replay can reuse a successful explicit canvas rather than repeat it. Queue deletion is not journal deletion. Session clear/reset must remove retained bodies and prevent orphan journal rows from resurrecting a cancelled answer; keep minimal create-attempt tombstones to preserve the at-most-one guarantee. No automatic expiry of attempt tombstones in this version.

No exactly-once guarantee is claimed for Slack message/file publication: their network outcomes can be ambiguous too. Save stage intent before writes, reconcile known message timestamps/upload IDs, never call completion twice for one ticket, and retain unresolved state for replay/operator recovery. Before a new file ticket after ambiguous completion, inspect known file shares (`files.info`) to avoid a needless duplicate; if inspection is unavailable, prefer full-text fallback and record uncertainty. An uncertain carrier post can leave a duplicate short link on recovery; it must never cause a second canvas or duplicate full answer stream.

Journal/storage unavailability means no canvas attempt. Try ordinary file/full-text delivery where current policy allows, and retain the queue row on unsuccessful send. Explicit tool returns an actionable error instead of creating without a fence. Policy revocation terminates all outward stages and retains the answer locally; it must not be bypassed as “fallback”. Propagate cancellation; do not turn a stopped request into a late artifact upload.

Logs contain stage, safe error class/Slack code, request digest, artifact ID and fallback outcome. Never log tokens, signed upload URLs, original text, summaries, titles, raw response bodies, canvas creation `detail` or raw exception text from HTTP. User output contains the answer and link/file, never API errors. If every send fails, record failure and leave delivery pending; the tool reports `Could not share a canvas in this Slack conversation. Send the write-up as a normal reply; Clawq will try file delivery. An admin can check the Slack scopes and connector logs.`

## Config and runtime split

```json
{"channels":{"slack":{"long_reply":{"mode":"canvas","threshold":2500}}}}
```

`Slack_config_types.long_reply_mode = Canvas | File | Off`; `long_reply = { mode; threshold }`. Missing object/fields use canvas/2500. Threshold is an integer in **1..40000**. Loader warns and defaults an invalid field independently; malformed object defaults both. Unknown mode loads as `Canvas` with a warning. `config set` validates both leaf values and the whole object, rejects unknown keys, nulls, floats, booleans and out-of-range thresholds with the full key and an example. Setting a parent `channels.slack`/`channels` object must run nested validation too, rather than bypass it. Serialization/config-show include the effective setting; search/tree include both leaves. Never print an untrusted invalid config value in a log.

At final delivery, read the manager's current config and freeze that reply's policy in its journal. Reload during model generation affects that reply; reload during delivery does not create another artifact or restart a selected stage, but security/credential/room gates refresh before each HTTP request. New replies use new settings. Tool registration uses a current-config accessor; reload must not replace it with a stale captured config.

Core: `Slack_config_types`, new `Slack_reply_types`, `Slack_long_reply_policy`, neutral new `Session_final_reply`, tool disabled placeholder and prompt/config plumbing. Integrations: new `Slack_canvas`, `Slack_file_upload`, `Slack_reply_store`, `Slack_reply_delivery`, `Slack_canvas_tool`, and extracted `Slack_message_transport`. All HTTP stays in `clawq_runtime_integrations`; core never references these modules. `Tools_builtin` registers a schema-compatible placeholder returning `slack_canvas_create is disabled in minimal build; use the full clawq binary.` Full daemon startup replaces it with the real tool. No new CLI command is needed.

Extract the existing message transport wrappers at the top of `slack.ml` into `Slack_message_transport` before hooking final sites; `slack.ml` must end smaller than its initial 1121 lines. Aim below 1000 lines per module, hard limit 2000; new modules must each stay below 1000. No circular dependency from core Session modules back into Slack integration code.

## Tool and prompt contract

Tool name: `slack_canvas_create`. Risk: `Medium` (publishes to current conversation). Non-streaming invocation, ordinary actionable tool errors. Schema:

```json
{
  "type":"object",
  "properties":{
    "title":{"type":"string","minLength":1,"maxLength":120,"description":"Short canvas title (required)."},
    "markdown":{"type":"string","minLength":1,"maxLength":1048576,"description":"Complete Markdown write-up (required)."}
  },
  "required":["title","markdown"],
  "additionalProperties":false
}
```

Exact description:

> Create a Slack canvas containing a complete Markdown write-up, share its link in the current Slack conversation, and give that conversation's recipients read access. Requires an authorized Slack turn; the destination comes from that turn. Use for long or structured answers and research reports. For short answers, reply normally. Supply title and markdown; on success use the returned permalink in a brief reply and do not repeat the write-up. The tool publishes a short title-and-link message to establish sharing. If canvas sharing fails, it returns an error so you can reply normally and let automatic file fallback preserve the answer.

Returns JSON `{"canvas_id":"F…","permalink":"https://…","shared_message_ts":"…"}` only after grant success. It does not silently return a file as a canvas. Share-carrier text for an explicit tool is escaped title plus labelled link; the subsequent model reply is normally short. Reuse an explicit receipt when the final answer is exactly the same write-up; a genuinely different long final is a separate logical reply. Tool access comes from `Slack_huddle_context.resolve` with snapshot/room checks, extended to accept DM/MPDM when authorized, not from tool arguments or a session-key guess. Queue replay may use its verified restored request context; synthetic work lacking that context cannot use the tool.

Outside Slack return: `slack_canvas_create requires an authorized Slack turn. Reply in the current conversation, or run this tool from Slack; do not supply a channel ID or token.` No network call occurs. Hide the tool in non-Slack room catalogs via neutral verified connector metadata, while keeping this invocation guard for cached calls. Never import the integration into core `Room_tool_availability`. Add it to `researcher` and `documenter` allowlists; review every other built-in's purpose rather than bulk-grant it to coding or reviewing agents. Regenerate built-ins with the gate's `gen-agents` command and verify drift.

Exact Slack prompt guidance in canvas mode:

> In Slack, final answers longer than {threshold} characters are automatically placed in a canvas, with a Markdown file fallback. Lead with a self-contained opening paragraph of at most 600 characters; that paragraph and the full-answer link appear in the conversation. For long or structured research write-ups, you may call slack_canvas_create with a title and complete Markdown. It shares a short link message here; then reply briefly with the result and link. Do not repeat the full write-up. Short answers should be ordinary replies.

In file mode replace the first sentence with `In Slack, final answers longer than {threshold} characters are automatically attached as Markdown files.` and “full-answer link” with “file attachment”. In off mode omit the automatic-overflow sentences and retain optional-tool advice only when available. Hide tool advice when template/room availability disallows it. Render from current turn connector + config at each model request, including after compaction/reload; ensure `prompt.dynamic_enabled=false` does not drop this channel delivery instruction. No non-Slack or session-less prompt receives Slack advice.

## Decisions

1. Final-only callback — protects notices/progress/relays — rejected low-level length interceptor.
2. Status-only streams and buffered answers — no full-answer duplication — rejected stream-then-replace.
3. Standalone canvas, then read grant — avoids tab/write side effects — rejected `channel_id` create and channel canvas reuse.
4. `files.info` permalink — uses returned authoritative URL — rejected hard-coded team URL construction; requires `files:read` in setup.
5. 600-scalar opening block — deterministic and bounded — rejected model summary and whole first block without a cap.
6. Durable create-attempt fence — at-most-one even on unknown success — rejected blind create retry and title searches.
7. User-level DM/MPDM grants — the documented access path — rejected unsupported DM `channel_ids`; membership-lifetime tradeoff is pending Max.
8. Exact Markdown file on unsupported canvas content — preserves the answer — rejected truncation or lossy table/HTML stripping.
9. Automatic modes do not disable explicit tool — separates delivery preference from deliberate publication — rejected treating `off` as a capability kill switch.
10. New upload-v2 adapter — absent in this checkout — rejected planning against presumed standing-order upload code.

Implementation acceptance includes the requirements' fake-HTTP cases, replay/crash boundaries, read-only grants, parent preservation and minimal-build separation. Connector reference (`docs/src/content/docs/channels.mdx`), setup (`docs/setup-guide.md`), config reference and `docs/public/llms-full.txt` are implementation tasks, not changes made during this design step.
