# Component Design

## `src/github_api.ml`

GitHub REST API client. All network calls use the existing `Http_client` module.

```ocaml
(** Resolve auth headers from the configured auth method.
    PAT: [("Authorization", "Bearer " ^ token)]
    Future GitHub App: fetches installation token, returns same shape. *)
val auth_headers : Runtime_config.github_auth -> (string * string) list Lwt.t

(** Post a comment on an issue or PR.
    Works for both: GitHub issues/{n}/comments handles PRs too. *)
val post_comment :
  auth:Runtime_config.github_auth ->
  owner:string ->
  repo:string ->
  issue_number:int ->
  body:string ->
  unit Lwt.t

(** Reply to a pull_request_review_comment in-thread.
    Uses /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies *)
val reply_to_review_comment :
  auth:Runtime_config.github_auth ->
  owner:string ->
  repo:string ->
  pull_number:int ->
  comment_id:int ->
  body:string ->
  unit Lwt.t

(** Get list of files changed in a PR.
    Returns (filename, status, additions, deletions) tuples.
    Handles pagination (up to 300 files). *)
val get_pr_files :
  auth:Runtime_config.github_auth ->
  owner:string ->
  repo:string ->
  pull_number:int ->
  (string * string * int * int) list Lwt.t
  (* (filename, status, additions, deletions) *)
```

### Rate limiting

Outgoing GitHub API calls are gated using a dedicated `Rate_limiter` instance for the repo.
Key = `"github:{owner}/{repo}"`. Rate: 1 req/s per repo (conservative, well within GitHub's limits).

On 403/429 response: log warning, extract `Retry-After` header if present, do not retry automatically in v1 (just log the error and return `unit`).

### Token redaction

`auth_headers` logs at Debug level with token redacted to first 8 chars: `ghp_xxxx...`.

---

## `src/github_webhook.ml`

Pure logic — no I/O, no Lwt. Highly testable.

```ocaml
(** Result of parsing a GitHub webhook event *)
type parsed_event =
  | PullRequest of pr_event
  | IssueComment of issue_comment_event
  | PrReviewComment of pr_review_comment_event
  | Ignored   (* event type we don't handle, or action we skip *)

type pr_event = {
  action : string;          (* "opened" | "edited" | "reopened" *)
  owner : string;
  repo : string;
  pr_number : int;
  pr_title : string;
  pr_body : string;
  pr_author : string;
  base_branch : string;
  head_branch : string;
  html_url : string;
}

type issue_comment_event = {
  owner : string;
  repo : string;
  issue_number : int;
  is_pr : bool;
  comment_id : int;
  comment_author : string;
  comment_body : string;
  issue_title : string;
  html_url : string;
}

type pr_review_comment_event = {
  owner : string;
  repo : string;
  pr_number : int;
  comment_id : int;
  comment_author : string;
  comment_body : string;
  in_reply_to_id : int option;  (* if replying to existing thread *)
  diff_hunk : string;
  file_path : string;
  pr_title : string;
  html_url : string;
}

(** Verify X-Hub-Signature-256 header.
    Expected format: "sha256=<hex>"
    Uses digestif.SHA256.hmac_string (constant-time comparison via eqaf). *)
val verify_signature :
  secret:string ->
  body:string ->
  signature_header:string ->
  bool

(** Parse a GitHub webhook event from X-GitHub-Event header + JSON body.
    Returns Ignored for unrecognized event types or unsupported actions. *)
val parse_event :
  event_type:string ->  (* value of X-GitHub-Event header *)
  body:string ->
  parsed_event

(** Extract /clawq trigger from the event's text content.
    Returns Some (user_message, full_context_preamble) if /clawq found.
    Returns None if no /clawq present — event should be silently ignored.

    user_message: text from /clawq prefix (stripped) to next blank line or EOF.
    full_context_preamble: structured markdown block with repo, PR, comment context.

    Only the FIRST /clawq occurrence is extracted. Subsequent ones are ignored. *)
val extract_clawq :
  event:parsed_event ->
  pr_files:(string * string * int * int) list ->  (* pre-fetched, may be [] *)
  (string * string) option   (* (user_message, full_context_preamble) *)

(** Deterministic session key for an event.
    "github:{owner}/{repo}:pr:{n}" for PR events and review comments.
    "github:{owner}/{repo}:issue:{n}" for issue-only comments. *)
val session_key : parsed_event -> string

(** Extract repo owner and name from a parsed event. *)
val repo_of_event : parsed_event -> string * string  (* (owner, repo) *)
```

### `/clawq` extraction algorithm

```
text = body of comment or PR description

1. Find first line starting with "/clawq" (case-insensitive, after stripping leading whitespace)
2. If not found → return None
3. user_message_raw = from that line (including "/clawq") to:
     - next completely blank line, OR
     - end of text
4. Strip "/clawq" prefix from first line, trim
5. Recombine remaining lines → user_message
6. Build full_context_preamble from event fields
7. Return Some (user_message, full_context_preamble)
```

### `full_context_preamble` format

```
## GitHub Context
Repository: {owner}/{repo}
{PR/Issue} #{n}: "{title}"
{PR: Author: @{author} | base: {base} → head: {head}}
State: {state}

{PR/Issue} Description:
  {body (truncated to 2000 chars if long)}

Event: {event type description}
{If review comment: File: {path}  [line {line}]}
{If review comment: Diff hunk:
  {diff_hunk}}
Full comment by @{author}:
  {full comment body}

{If pr_files non-empty:
Changed files ({count}):
{  - filename (+additions -deletions)  for each file, up to 20}
{  ... and N more  if >20}

To inspect the full diff: `gh pr diff {n} --repo {owner}/{repo}`}
PR URL: {html_url}
```

---

## `src/github.ml`

Orchestration layer. Analogous to `slack.ml`.

```ocaml
(** Handle a single webhook HTTP request for a given repo config.
    Returns the HTTP response body string (always 200 unless signature fails). *)
val handle_webhook :
  repo_config:Runtime_config.github_repo_config ->
  github_config:Runtime_config.github_config ->
  session_manager:Session.t ->
  api_limiter:Rate_limiter.t ->
  event_type:string ->
  body:string ->
  headers:Cohttp.Header.t ->
  string Lwt.t

(** Format the reply comment body.
    Includes blockquote of the /clawq command and the agent response. *)
val format_reply :
  command:string ->   (* the extracted /clawq command text *)
  response:string ->
  string
```

### handle_webhook flow

```
1. Extract X-Hub-Signature-256 header
2. verify_signature ~secret:repo_config.webhook_secret → 403 on failure
3. parse_event ~event_type ~body
4. Check react_to filter (is this event type in repo_config.react_to?)
5. If event = Ignored or filtered → return "ok" (200)
6. Check allow_users filter (is comment author in repo_config.allow_users?)
7. extract_clawq → if None → return "ok" (200)
8. If include_pr_files: rate-limit check → get_pr_files (non-blocking, skip if limited)
9. build context preamble from event + pr_files
10. session_key = Github_webhook.session_key event
11. full_message = context_preamble ^ "\n\n" ^ user_message
12. Session.turn ~key:session_key ~message:full_message
13. Format reply: format_reply ~command:user_message ~response
14. Post reply:
    - PrReviewComment → reply_to_review_comment (in-thread)
    - PullRequest, IssueComment → post_comment (top-level)
15. Log: Logs.info "GitHub: {repo} {event_type} #{n} by @{user} → replied"
16. Return "ok"
```

### Error handling

- Agent error → post "Sorry, an error occurred. Please try again." as comment, log error
- API post failure → log error, don't retry (leave it to user to notice)
- pr_files fetch failure → continue without files (non-fatal, just omit section)
- Signature failure → return 403 JSON `{"error":"invalid signature"}`, log warn

---

## `src/cf_tunnel.ml`

Cloudflared tunnel management.

```ocaml
type tunnel_state =
  | NotConfigured
  | StaticUrl of string
  | Managed of managed_state

type managed_state = {
  url : string;
  pid : int;
  restart_count : int;
}

(** Resolve tunnel URL from config/env — synchronous, no subprocess.
    Returns Some url if tunnel.url or CLAWQ_TUNNEL_URL is set. *)
val resolve_static : config:Runtime_config.tunnel_config -> string option

(** Start managed cloudflared named tunnel.
    Spawns `cloudflared tunnel run {tunnel_name}` using Lwt_process.
    Parses stdout/stderr for the tunnel URL line.
    Returns Lwt promise that resolves to Some url when URL is known,
    or None if cloudflared fails to start within 30 seconds.

    Restarts on exit with exponential backoff (1s, 2s, 4s, max 60s).
    Logs all cloudflared output at Debug level.
    Logs tunnel URL and restart events at Info level. *)
val start_managed :
  config:Runtime_config.tunnel_config ->
  on_url:(string -> unit) ->
  unit Lwt.t   (* never returns — runs tunnel supervisor loop *)

(** Combined: resolve static first, then start managed if configured.
    Calls on_url callback whenever URL is known (immediately for static,
    after subprocess start for managed).
    Also returns initial URL as option (None if managed and not yet started). *)
val start :
  config:Runtime_config.tunnel_config ->
  on_url:(string -> unit) ->
  string option * (unit Lwt.t)
  (* (initial_url_if_static, supervisor_promise) *)
```

### Managed tunnel implementation

**Critical finding from research**: Named tunnels do NOT print a URL at runtime. The URL is the DNS hostname configured via `cloudflared tunnel route dns`. Therefore managed mode works as follows:

- Spawn cloudflared, watch stderr for readiness signal: 4× `Connection registered connIndex=N` lines
- Once ready, call `on_url config.tunnel.url` — URL must be set in config (always true for named tunnels)
- If `config.tunnel.url` is empty + managed=true: log error, skip subprocess (can't determine URL)

```ocaml
(* Command: *)
(* cloudflared tunnel --no-autoupdate --grace-period 5s run {tunnel_name} *)
(* With explicit config dir: *)
(* cloudflared --config {config_dir}/config.yml tunnel --no-autoupdate --grace-period 5s run {tunnel_name} *)

(* Readiness signal: count lines matching "Connection registered connIndex=" *)
(* At 4 matches → tunnel is up → call on_url config.tunnel.url *)
(* Example stderr lines: *)
(*   2024-01-15T10:23:45Z INF Connection registered connIndex=0 ip=198.41.200.73 *)
(*   2024-01-15T10:23:45Z INF Connection registered connIndex=1 ip=198.41.192.57 *)
(*   2024-01-15T10:23:45Z INF Connection registered connIndex=2 ip=198.41.200.43 *)
(*   2024-01-15T10:23:45Z INF Connection registered connIndex=3 ip=198.41.192.33 *)

(* Key flags: *)
(*   --no-autoupdate  : disable cloudflared self-restart (we manage restarts) *)
(*   --grace-period 5s: faster graceful drain on SIGTERM *)
(*   --pidfile {path} : optional readiness file, written after first connection *)

(* Note: cloudflared >= 2025.6.1 supports --output json for structured stderr *)
(* Not required for v1; text-mode parsing above is sufficient and version-agnostic *)
```

### Named tunnel prerequisites (documented for users)

Before `tunnel.managed = true` + `tunnel.tunnel_name = "myname"` works:
1. `cloudflared login` (authenticates to Cloudflare account)
2. `cloudflared tunnel create myname` (creates tunnel, writes credentials to `~/.cloudflared/`)
3. `cloudflared tunnel route dns myname myname.example.com` (creates DNS CNAME)
4. Set `tunnel.url = "https://myname.example.com"` OR let managed mode read from output

---

## `src/http_server.ml` changes

New optional parameter:

```ocaml
val start :
  port:int ->
  host:string ->
  require_pairing:bool ->
  auth_token:string option ->
  session_manager:Session.t ->
  ?slack_config:Runtime_config.slack_config ->
  ?github_config:Runtime_config.github_config ->
  ?github_api_limiter:Rate_limiter.t ->
  ?ip_limiter:Rate_limiter.t ->
  ?session_limiter:Rate_limiter.t ->
  unit ->
  unit Lwt.t
```

New route in handler:

```ocaml
| `POST, path when is_github_webhook_path path github_config ->
    let repo_config = lookup_github_repo path (Option.get github_config) in
    let event_type =
      match Cohttp.Header.get (Cohttp.Request.headers req) "x-github-event" with
      | Some v -> v
      | None -> ""
    in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let* result =
      Github.handle_webhook ~repo_config
        ~github_config:(Option.get github_config)
        ~session_manager
        ~api_limiter:(Option.get github_api_limiter)
        ~event_type ~body:body_str
        ~headers:(Cohttp.Request.headers req)
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:json_headers
      ~body:result ()
```

Helper:
```ocaml
let is_github_webhook_path path = function
  | None -> false
  | Some gc ->
      List.exists (fun r -> r.Runtime_config.webhook_path = path) gc.Runtime_config.repos
```

---

## `src/daemon.ml` changes

```ocaml
(* After session_manager creation: *)
let tunnel_url_ref = ref None in
let tunnel_supervisor =
  if config.tunnel.enabled then begin
    let (initial_url, supervisor) =
      Cf_tunnel.start ~config:config.tunnel ~on_url:(fun url ->
        tunnel_url_ref := Some url;
        Logs.info (fun m -> m "Tunnel URL: %s" url);
        (match config.channels.github with
         | Some _ ->
             Logs.info (fun m ->
               m "GitHub webhooks ready at: %s/github/webhook/..." url)
         | None -> ()))
    in
    tunnel_url_ref := initial_url;
    supervisor
  end else
    Lwt.return_unit
in

(* Wire github_api_limiter *)
let github_api_limiter =
  Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0
in

(* In Http_server.start call, add: *)
?github_config:config.channels.github
~github_api_limiter

(* Lwt.async for tunnel supervisor: *)
Lwt.async (fun () ->
  Lwt.catch
    (fun () -> tunnel_supervisor)
    (fun exn ->
      Logs.err (fun m -> m "Tunnel supervisor error: %s" (Printexc.to_string exn));
      Lwt.return_unit));

(* Channels log line, add github: *)
Logs.info (fun m ->
  m "Channels: cli=%b telegram=%b discord=%b slack=%b github=%b"
    config.channels.cli
    (config.channels.telegram <> None)
    (config.channels.discord <> None)
    (config.channels.slack <> None)
    (config.channels.github <> None));

(* write_state, add: *)
("github_enabled", `Bool (config.channels.github <> None));
("tunnel_url", match !tunnel_url_ref with Some u -> `String u | None -> `Null);
```

---

## `test/test_github.ml`

Test suites:

1. **`github_webhook_sig`** — signature verification
   - valid signature accepted
   - invalid signature rejected
   - wrong secret rejected
   - malformed header rejected

2. **`github_webhook_parse`** — event parsing
   - pull_request opened/edited/reopened parsed correctly
   - pull_request closed → Ignored
   - issue_comment on PR parsed correctly
   - issue_comment on issue parsed correctly
   - pr_review_comment parsed correctly with diff_hunk
   - unknown event type → Ignored
   - malformed JSON → Ignored

3. **`github_webhook_extract`** — `/clawq` extraction
   - `/clawq` in PR body → extracted
   - `/clawq` in comment → extracted
   - `/clawq` mid-comment (after preamble text) → extracted
   - `/clawq` multi-paragraph → first paragraph only
   - multiple `/clawq` in one comment → first one only
   - no `/clawq` → None
   - `/clawq` only (no trailing text) → Some ("", ...)
   - `/CLAWQ` case-insensitive → extracted
   - leading whitespace before `/clawq` → extracted

4. **`github_session_key`** — session key generation
   - PR events → `github:owner/repo:pr:{n}`
   - issue comment (non-PR) → `github:owner/repo:issue:{n}`
   - PR issue comment → `github:owner/repo:pr:{n}`
   - review comment → `github:owner/repo:pr:{n}`

5. **`github_http_handler`** — HTTP route tests
   - unknown path → 404
   - bad signature → 403
   - no X-GitHub-Event → 200 (ignored)
   - valid ping event → 200
   - no /clawq in body → 200 (ignored)
   - valid /clawq → 200 (mock session turn called)

Use `api-examples/github/*.json` payloads as test fixtures.
