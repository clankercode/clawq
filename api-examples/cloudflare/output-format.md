# cloudflared Output Format

## Output Stream

All cloudflared log output goes to **stderr**. stdout is empty during normal operation (used only by a few subcommands like `cloudflared tail` for streamed log data). When spawning cloudflared as a child process, capture its stderr.

## Text Log Format (Default)

```
YYYY-MM-DDTHH:MM:SSZ LEVEL message [key=value ...]
```

- Timestamp: RFC3339 UTC (`Z` suffix), no sub-second precision.
- Level: exactly 3 characters, one of: `INF`, `WRN`, `ERR`, `DBG`.
- Message: free-form text.
- Key=value pairs for structured fields follow the message, space-separated.
- Uses `zerolog` internally for structured logging.

## Quick Tunnel Full Startup Example (text mode)

This is what to expect on stderr when running `cloudflared tunnel --url http://localhost:8080`:

```
2024-10-04T15:35:09Z INF Requesting new quick Tunnel on trycloudflare.com...
2024-10-04T15:35:15Z INF +--------------------------------------------------------------------------------------------+
2024-10-04T15:35:15Z INF | Your quick Tunnel has been created! Visit it at (it may take some time to be reachable): |
2024-10-04T15:35:15Z INF | https://estate-quilt-cfr-forces.trycloudflare.com                                        |
2024-10-04T15:35:15Z INF +--------------------------------------------------------------------------------------------+
2024-10-04T15:35:15Z INF Version 2024.9.1
2024-10-04T15:35:15Z INF GOOS: darwin, GOVersion: go1.22.2-devel-cf, GoArch: amd64
2024-10-04T15:35:15Z INF Settings: map[url:http://localhost:8080]
2024-10-04T15:35:16Z INF Connection registered connIndex=0 location=IAD
2024-10-04T15:35:16Z INF Connection registered connIndex=1 location=ATL
2024-10-04T15:35:16Z INF Connection registered connIndex=2 location=ORD
2024-10-04T15:35:16Z INF Connection registered connIndex=3 location=DFW
```

## Named Tunnel Startup Example (text mode)

Running `cloudflared tunnel run my-tunnel`:

```
2024-12-05T10:30:15Z INF Starting tunnel tunnelID=a7b3c4d5-e6f7-8901-2345-6789abcdef01
2024-12-05T10:30:16Z INF Connection registered connIndex=0 location=IAD
2024-12-05T10:30:17Z INF Connection registered connIndex=1 location=ATL
2024-12-05T10:30:18Z INF Connection registered connIndex=2 location=ORD
2024-12-05T10:30:19Z INF Connection registered connIndex=3 location=DFW
```

## Request Log Lines (during operation)

```
2026-02-06T10:30:00Z INF Request connection connIndex=0 ip=198.41.200.10 location=DFW
```

## Error During Operation

```
2026-02-06T10:30:02Z ERR error proxying request to origin error="connection refused" connIndex=0
```

## Graceful Shutdown Log Sequence

Expected on SIGTERM (exit code 143 = 128+15, or 0 if handler exits cleanly):

```
2022-09-22T14:16:55Z INF Initiating graceful shutdown due to signal terminated ...
2022-09-22T14:16:56Z INF Unregistered tunnel connection connIndex=0
2022-09-22T14:16:56Z INF Unregistered tunnel connection connIndex=1
2022-09-22T14:16:56Z INF Unregistered tunnel connection connIndex=2
2022-09-22T14:16:56Z INF Unregistered tunnel connection connIndex=3
2022-09-22T14:16:56Z INF Tunnel server stopped
2022-09-22T14:16:56Z INF Metrics server stopped
```

Note: cloudflared also emits ~8-9 ERR-level log lines during normal graceful shutdown due to a known bug (GitHub issue #1038). These are not actual errors. Examples:
```
ERR writing finish: Application error 0x0 (remote)
ERR Failed to serve quic connection error=...context canceled
ERR no more connections active and exiting
ERR icmp router terminated error='context canceled'
```
These can be filtered when detecting shutdown by checking for the `INF Initiating graceful shutdown` line first.

## Regex Patterns for URL Extraction

For quick tunnel URL (scan each stderr line):

```
# Minimal — match the URL anywhere in a line
https://[a-z0-9-]+\.trycloudflare\.com

# Strict — require the URL is in the box line
INF \| (https://[a-z0-9-]+\.trycloudflare\.com)\s*\|

# OCaml Re/PCRE style (capturing group):
{|https://([a-z0-9-]+\.trycloudflare\.com)|}
```

For "tunnel is ready" detection (named tunnels):
```
# All 4 connections registered = tunnel is up
INF Connection registered connIndex=3
```

For shutdown detection:
```
INF Initiating graceful shutdown
```

## JSON Log Format (cloudflared >= 2025.6.1)

As of version 2025.6.1, JSON output to stdout is available via:
```sh
cloudflared tunnel --output json --url http://localhost:PORT
```

Flag: `--output json`

Before 2025.6.1, `--logfile <path>` writes JSON to a file (not stdout). The `--output json` flag was implemented as TUN-9371, released 2025-06-16.

JSON line format (approximate, based on zerolog):
```json
{"level":"info","time":"2024-10-04T15:35:15Z","message":"Your quick Tunnel has been created!","url":"https://example.trycloudflare.com"}
```

The exact JSON field names depend on zerolog's output. Use `--output json` only if you can guarantee cloudflared >= 2025.6.1; otherwise parse text stderr.

## Version Applicability

- Text format behavior: stable across all versions.
- `--output json`: requires >= 2025.6.1 (added TUN-9371, 2025-06-16).
- `--logfile` JSON to file: available in older versions.

## Sources

- https://github.com/cloudflare/cloudflared/issues/1033
- https://github.com/cloudflare/cloudflared/issues/1038
- https://oneuptime.com/blog/post/2026-02-06-cloudflare-tunnel-logs-collector/view
- https://developers.cloudflare.com/cloudflare-one/networks/connectors/cloudflare-tunnel/monitor-tunnels/logs/
