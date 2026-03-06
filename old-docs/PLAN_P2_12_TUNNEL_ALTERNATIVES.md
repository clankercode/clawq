# P2-12: Tunnel Alternatives — Tailscale & ngrok

## Context

clawq currently supports only Cloudflare Tunnel (`tunnel_cloudflare.ml`). nullclaw additionally supports Tailscale and ngrok. Multiple tunnel options matter because:
- Cloudflare Tunnel requires a Cloudflare account (though `trycloudflare.com` is account-free for quick tunnels)
- Tailscale is preferred in corporate/homelab environments (mesh VPN, no public exposure)
- ngrok has a generous free tier and is widely known by developers

This plan adds Tailscale and ngrok as tunnel backends behind the existing `Tunnel` interface.

## Current Tunnel Interface

From `Interfaces.v` (Coq):
```
Tunnel: { name, start, status, endpoint }
```

In OCaml (`tunnel_cloudflare.ml` pattern):
- `start : config:Runtime_config.tunnel_config -> unit Lwt.t` — spawns `cloudflared` process, monitors output for URL
- `status : unit -> string` — returns current endpoint URL or "not running"

## Design

### Tunnel Backend Selection

Add `backend` field to `tunnel_config`:
```ocaml
type tunnel_config = {
  enabled : bool;
  backend : string;    (* "cloudflare" | "tailscale" | "ngrok" | "custom" *)
  port : int;          (* local gateway port to expose *)
  (* Cloudflare-specific *)
  cloudflare : cloudflare_tunnel_config option;
  (* Tailscale-specific *)
  tailscale : tailscale_tunnel_config option;
  (* ngrok-specific *)
  ngrok : ngrok_tunnel_config option;
  (* Custom *)
  custom : custom_tunnel_config option;
}
```

Dispatch in `cmd_tunnel` and daemon based on `backend` field.

---

## Part A: Tailscale Funnel

### How It Works

Tailscale Funnel exposes a local port to the public internet via Tailscale's infrastructure. Requires `tailscale` CLI and an active Tailscale session.

```bash
tailscale funnel --bg {port}
```

Returns a `https://{hostname}.ts.net` URL. The `--bg` flag runs it in the background.

Status:
```bash
tailscale funnel status
```

Stop:
```bash
tailscale funnel --bg off
```

### Config

```ocaml
type tailscale_tunnel_config = {
  tailscale_path : string;   (* default "tailscale" *)
  https_only : bool;         (* default true — enforce HTTPS via funnel *)
}
```

### Implementation: `src/tunnel_tailscale.ml`

```ocaml
let start ~config =
  let port = config.tunnel.port in
  let args = [| "funnel"; "--bg"; string_of_int port |] in
  let proc = Lwt_process.open_process_in ("tailscale", args) in
  (* Read output for URL: "https://hostname.ts.net" *)
  read_url_from_output proc

let status () =
  (* Run: tailscale funnel status *)
  (* Parse output for active funnels and URLs *)
  ...
```

**URL extraction:** Parse output for `https://` prefix line. Tailscale Funnel output format:
```
Funnel on:
  https://mymachine.tail12345.ts.net:{port}
```

**Tunnel command integration:** `cmd_tunnel` already handles `create`, `delete`, `status` subcommands for Cloudflare. Same pattern works for Tailscale with different subprocess.

### External Requirement

`tailscale` CLI must be installed and authenticated (`tailscale up`). `cmd_doctor` should check for this if Tailscale tunnel is configured.

---

## Part B: ngrok

### How It Works

ngrok exposes a local port via `ngrok http {port}`, creating a `https://*.ngrok-free.app` (free tier) or custom domain URL.

**Two modes:**
1. **Process mode** (simple): spawn `ngrok http {port}` and query the local ngrok API for the URL
2. **API mode**: use ngrok's REST API with auth token for programmatic control

For clawq: process mode (matches nullclaw's approach and works without API key).

```bash
ngrok http {port} --log stdout --log-format json
```

ngrok logs JSON events including `url` when tunnel is established. Also provides a local API at `http://localhost:4040/api/tunnels`.

### Config

```ocaml
type ngrok_tunnel_config = {
  ngrok_path : string;       (* default "ngrok" *)
  auth_token : string option; (* for ngrok Pro/Teams features *)
  region : string option;    (* e.g. "eu", "ap"; default = auto *)
  subdomain : string option; (* paid feature *)
}
```

### Implementation: `src/tunnel_ngrok.ml`

```ocaml
let start ~config =
  let port = config.tunnel.port in
  let args = ref [| "http"; string_of_int port; "--log"; "stdout"; "--log-format"; "json" |] in
  let proc = Lwt_process.open_process_in ("ngrok", !args) in
  (* Read JSON log lines until we see {"msg":"started tunnel","url":"https://..."} *)
  read_url_from_json_log proc

let status () =
  (* Query http://localhost:4040/api/tunnels → parse public_url *)
  Http_client.get "http://localhost:4040/api/tunnels" >>= fun body ->
  let json = Yojson.Safe.from_string body in
  (* Extract tunnels[0].public_url *)
  ...
```

**URL extraction from JSON logs:**
```json
{"level":"info","msg":"started tunnel","name":"command_line","url":"https://abc123.ngrok-free.app"}
```

Or poll `http://localhost:4040/api/tunnels` after startup.

---

## Part C: Custom Tunnel

Some users run their own reverse proxy (e.g., `ssh -R`, `bore`, `localtunnel`). Support a generic custom tunnel backend that runs an arbitrary command and extracts a URL from its output via regex.

### Config

```ocaml
type custom_tunnel_config = {
  command : string;          (* e.g., "bore local {port} --to bore.pub" *)
  url_pattern : string;      (* regex to extract URL from stdout, e.g. "https://[^\s]+" *)
}
```

### Implementation: `src/tunnel_custom.ml`

```ocaml
let start ~config =
  let cmd = substitute_port config.custom.command config.tunnel.port in
  let proc = Lwt_process.open_process_in ("/bin/sh", [| "-c"; cmd |]) in
  let re = Re.Pcre.re config.custom.url_pattern |> Re.compile in
  (* Read lines, apply regex, extract first match as URL *)
  extract_url_from_output re proc
```

---

## Changes to Existing Files

- **Create**: `src/tunnel_tailscale.ml`
- **Create**: `src/tunnel_ngrok.ml`
- **Create**: `src/tunnel_custom.ml`
- **Modify**: `src/runtime_config.ml` — extend `tunnel_config` with backend + per-backend sub-configs
- **Modify**: `src/config_loader.ml` — parse new tunnel config fields
- **Modify**: `src/command_bridge.ml` — `cmd_tunnel` dispatches based on `backend`
- **Modify**: `src/service.ml` — start correct tunnel backend in service
- **Modify**: `src/dune` — add new modules to `clawq_runtime_integrations`

## Test Strategy

1. **Tailscale**: command construction (`tailscale funnel --bg 8080`), URL extraction from mock output
2. **ngrok**: JSON log line parser → URL extraction; local API response parse
3. **Custom**: `{port}` substitution in command string; regex URL extraction from mock output
4. **Backend dispatch**: `backend = "tailscale"` → Tailscale module called; etc.
5. **Config parse**: all sub-config fields parsed correctly; missing optional fields use defaults

Run: `make test`. Integration tests (process spawning) skipped if binary not in PATH.

## Dependencies

- `re` or `re.pcre` for custom tunnel URL pattern matching — check if already in deps; if not, add to `clawq_runtime_integrations`
- No other new deps. All subprocess management via `Lwt_process`.
- External tools: `tailscale`, `ngrok` (user-installed, optional)
