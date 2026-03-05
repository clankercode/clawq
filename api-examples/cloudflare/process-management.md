# cloudflared Subprocess Management

Notes for spawning cloudflared from an OCaml Lwt daemon, reading its output, and restarting on failure.

## Signal Handling

### SIGTERM / SIGINT

cloudflared handles both SIGTERM and SIGINT with graceful shutdown:

1. Stops accepting new requests.
2. Waits for in-progress requests to drain.
3. Drains closes after `--grace-period` timeout (default: 30s), OR when a second SIGTERM/SIGINT is received.
4. Logs `INF Initiating graceful shutdown due to signal terminated ...`
5. Logs each `INF Unregistered tunnel connection connIndex=N`.
6. Logs `INF Tunnel server stopped` and `INF Metrics server stopped`.
7. Exits.

To force immediate shutdown: send SIGTERM twice, or send SIGKILL after the grace period.

### Exit Codes

- Normal exit after SIGTERM: exit code **143** (128 + signal 15). This is the default POSIX behavior.
- If the process installs a custom handler that calls `os.Exit(0)`, it may exit 0 — but this is not the default for cloudflared.
- Treat exit code 143 as normal/expected when you sent SIGTERM.
- Any other non-zero exit code indicates an unexpected crash.

### SIGKILL

cloudflared does not handle SIGKILL (no process can). Use SIGKILL only as last resort after grace period.

## Key Flags for Subprocess Use

### Logging Flags

```
--loglevel <level>
```
Values: `debug`, `info` (default), `warn`, `error`, `fatal`.
Environment variable: `TUNNEL_LOGLEVEL`.

At `debug` level: logs request URLs, methods, all headers. Exposes sensitive data; avoid in production.

```
--logfile <path>
```
Writes logs to a file in **JSON format** (not the text format used for console/stderr).
Environment variable: `TUNNEL_LOGFILE`.
Mainly useful for persistent log archives or when you want structured JSON separately from stderr text.

```
--log-directory <path>
```
Directory for log files (rolling log files).
Environment variable: `TUNNEL_LOGDIRECTORY`.

```
--output json
```
Writes **JSON-formatted** logs to stdout instead of text to stderr.
Requires cloudflared >= 2025.6.1 (added 2025-06-16, TUN-9371).
Use this for structured log parsing if you can guarantee the version.

```
--transport-loglevel <level>
```
Controls verbosity of transport-layer logs separately from application logs.
Default: `warn`. Environment variable: `TUNNEL_PROTO_LOGLEVEL`.

### Process Management Flags

```
--pidfile <path>
```
Writes the process PID to a file after the first successful connection to Cloudflare.
Environment variable: `TUNNEL_PIDFILE`.
Useful to verify tunnel is up before proceeding (file appears = first connection established).

```
--grace-period <duration>
```
Time to wait for in-progress requests to drain after SIGTERM before force-closing.
Default: `30s`.
Environment variable: `TUNNEL_GRACE_PERIOD`.
For daemon use: set shorter if you want faster restart (e.g., `--grace-period 5s`).

## OCaml Lwt Subprocess Approach

### Spawning with Lwt_process

cloudflared writes the tunnel URL to **stderr**. Use `Lwt_process.open_process` with stderr captured:

```ocaml
(* Capture both stdout and stderr separately *)
let cmd = Lwt_process.shell "cloudflared tunnel --url http://localhost:8080 2>&1"
(* Or use open_process_full to get separate handles *)
let proc = Lwt_process.open_process_full
  ("cloudflared", [| "cloudflared"; "tunnel"; "--url"; "http://localhost:8080" |])
```

With `open_process_full`, the process record has `.stdout`, `.stderr`, `.stdin` channels.
Read from `.stderr` to find the tunnel URL.

### Reading the URL

Scan stderr lines until the `trycloudflare.com` URL appears:

```ocaml
let url_re = Re.(compile (seq [
  str "https://";
  rep1 (alt [alnum; char '-']);
  str ".trycloudflare.com"
]))

let rec find_url ic =
  let%lwt line = Lwt_io.read_line ic in
  match Re.exec_opt url_re line with
  | Some m -> Lwt.return (Re.Group.get m 0)
  | None -> find_url ic
```

### Detecting Named Tunnel Ready

For named tunnels (no URL to find), watch for all 4 connections registered:

```ocaml
(* count lines matching "INF Connection registered connIndex=" *)
(* tunnel is ready when count reaches 4 *)
```

### Restart on Failure

```ocaml
let rec run_cloudflared_with_restart () =
  let proc = Lwt_process.open_process_full (...) in
  let%lwt () = (* read URL from stderr, set it somewhere *) in
  let%lwt status = proc#status in
  (match status with
  | Unix.WEXITED 143 -> (* normal SIGTERM, we sent it *) Lwt.return ()
  | Unix.WEXITED 0 -> (* unexpected clean exit, restart *) run_cloudflared_with_restart ()
  | _ -> (* crash, log and restart after delay *)
    let%lwt () = Lwt_unix.sleep 5.0 in
    run_cloudflared_with_restart ())
```

### Auto-Update Behavior

By default cloudflared periodically checks for updates and **restarts itself** by spawning a new process. When the new process connects successfully, the old one gracefully drains and exits. From the parent's perspective, the subprocess exits (exit code 143 or 0). The daemon should restart cloudflared when it exits unexpectedly.

To disable auto-update: `cloudflared tunnel --no-autoupdate run ...`
Flag: `--no-autoupdate` (or set `no-autoupdate: true` in config.yml).

## Recommended Flags for Daemon Use

```sh
cloudflared tunnel \
  --loglevel info \
  --no-autoupdate \
  --grace-period 5s \
  --pidfile /run/cloudflared.pid \
  --url http://localhost:PORT
```

For named tunnel:
```sh
cloudflared tunnel \
  --config ~/.cloudflared/config.yml \
  --loglevel info \
  --no-autoupdate \
  --grace-period 5s \
  --pidfile /run/cloudflared.pid \
  run my-tunnel
```

## Parseable Output Strategy

Two options depending on cloudflared version:

### Option A: Text stderr parsing (all versions)

- Capture stderr line by line.
- Scan for `https://[a-z0-9-]+\.trycloudflare\.com` to get quick tunnel URL.
- Scan for `INF Connection registered connIndex=3` to confirm named tunnel is ready.
- Scan for `INF Initiating graceful shutdown` to detect shutdown.
- Ignore ERR lines after graceful shutdown initiation (known spurious errors during drain).

### Option B: JSON stdout (cloudflared >= 2025.6.1)

```sh
cloudflared tunnel --output json --url http://localhost:PORT
```

- All logs go to stdout as JSON lines (one JSON object per line).
- Parse with a JSON library; look for `"message"` field containing URL or status.
- Stdout is now the log stream; stderr may be empty or have minimal output.

## Known Issues

- cloudflared emits ~8-9 ERR-level log lines during normal graceful shutdown (GitHub issue #1038). These are benign. Filter them by checking whether `INF Initiating graceful shutdown` preceded them.
- After several hours, cloudflared may restart itself for auto-updates (emits `INF Initiating graceful shutdown due to signal terminated`). With `--no-autoupdate` this is suppressed.
- First connection log lines (`Connection registered`) may briefly show WRN if protocol negotiation takes time; this is normal.

## Sources

- https://developers.cloudflare.com/cloudflare-one/networks/connectors/cloudflare-tunnel/configure-tunnels/run-parameters/
- https://developers.cloudflare.com/cloudflare-one/networks/connectors/cloudflare-tunnel/configure-tunnels/cloudflared-parameters/run-parameters/
- https://github.com/cloudflare/cloudflared/issues/198
- https://github.com/cloudflare/cloudflared/issues/1038
- https://github.com/cloudflare/cloudflared/issues/1033
- https://deepwiki.com/cloudflare/cloudflared/2-tunnel-command
