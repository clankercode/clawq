# cloudflared Quick Tunnel

## CLI Syntax

```sh
cloudflared tunnel --url http://localhost:PORT
```

No Cloudflare account required. Produces a temporary `trycloudflare.com` URL.

Older/alternative form (equivalent):
```sh
cloudflared --url http://localhost:PORT
```

## Output Stream

All log output — including the tunnel URL announcement — is written to **stderr**, not stdout.
When spawning as a subprocess, read from the child's stderr file descriptor.

## Exact Log Output on Startup

The URL is printed inside a bordered box. Example (as seen in real usage):

```
2024-10-04T15:35:09Z INF Requesting new quick Tunnel on trycloudflare.com...
2024-10-04T15:35:15Z INF +--------------------------------------------------------------------------------------------+
2024-10-04T15:35:15Z INF | Your quick Tunnel has been created! Visit it at (it may take some time to be reachable): |
2024-10-04T15:35:15Z INF | https://estate-quilt-cfr-forces.trycloudflare.com                                        |
2024-10-04T15:35:15Z INF +--------------------------------------------------------------------------------------------+
2024-10-04T15:35:15Z INF Version 2024.9.1
2024-10-04T15:35:15Z INF GOOS: darwin, GOVersion: go1.22.2-devel-cf, GoArch: amd64
2024-10-04T15:35:15Z INF Settings: map[...url:http://localhost:8000]
```

After the URL box, cloudflared establishes 4 connections to Cloudflare edge PoPs:

```
2024-10-04T15:35:16Z INF Connection registered connIndex=0 location=IAD
2024-10-04T15:35:16Z INF Connection registered connIndex=1 location=ATL
2024-10-04T15:35:16Z INF Connection registered connIndex=2 location=ORD
2024-10-04T15:35:16Z INF Connection registered connIndex=3 location=DFW
```

All 4 `Connection registered` lines appearing means the tunnel is fully operational.

## Log Line Format

```
YYYY-MM-DDTHH:MM:SSZ LEVEL message [key=value ...]
```

- Timestamp: RFC3339 UTC with `Z` suffix, no sub-second precision.
- Level tokens: `INF`, `WRN`, `ERR`, `DBG` (debug only with `--loglevel debug`).
- No parentheses, brackets, or separators around the level token in text mode.
- Key=value pairs appended for structured fields (e.g., `connIndex=0 location=DFW`).

## URL Pattern

Subdomain is multiple lowercase hyphenated words:

```
https://word-word-word-word.trycloudflare.com
```

Examples:
- `https://threaded-fathers-explore-supplier.trycloudflare.com`
- `https://estate-quilt-cfr-forces.trycloudflare.com`
- `https://gba-miracle-enforcement-oct.trycloudflare.com`

## Regex to Extract URL from stderr Lines

Simple (recommended):
```
https://[a-z0-9-]+\.trycloudflare\.com
```

From the box line specifically (captures group 1 as the URL):
```
INF \| (https://[a-z0-9-]+\.trycloudflare\.com)\s*\|
```

More precise subdomain pattern:
```
https://([a-z][a-z0-9]*(?:-[a-z0-9]+)+)\.trycloudflare\.com
```

## Legal Notice

On startup cloudflared prints a notice that quick tunnels have no uptime guarantee and are covered by Cloudflare's Terms of Service. This is printed to stderr before the URL box.

## Tunnel Lifetime

The tunnel lives only as long as the cloudflared process runs. There is no persistent DNS record.

## Sources

- https://deepwiki.com/cloudflare/cloudflared/2.4-quick-tunnels
- https://github.com/cloudflare/cloudflared/issues/793
- https://github.com/cloudflare/cloudflared/issues/866
- https://www.davidma.co/blog/2025-08-06-cloudflare-tunnel/
