# cloudflared Named Tunnel

Named tunnels are persistent, require a Cloudflare account, and use pre-configured DNS routing. Unlike quick tunnels, the public URL is not auto-generated — it is whatever hostname you configured via DNS routing.

## Prerequisites

1. A Cloudflare account with a domain under Cloudflare DNS management.
2. `cloudflared` authenticated: `cloudflared tunnel login` (saves credentials to `~/.cloudflared/cert.pem`).
3. A named tunnel created: `cloudflared tunnel create <NAME>` (saves credentials JSON).
4. DNS route configured: `cloudflared tunnel route dns <NAME> <hostname>`.
5. A `config.yml` file.

## Creating a Named Tunnel

```sh
# Login (opens browser, saves cert.pem)
cloudflared tunnel login

# Create tunnel (saves <UUID>.json credentials file)
cloudflared tunnel create my-tunnel

# Route a DNS hostname to the tunnel
cloudflared tunnel route dns my-tunnel tunnel.example.com
```

## Running a Named Tunnel

Basic (uses config from default search paths):
```sh
cloudflared tunnel run my-tunnel
```

With explicit config file:
```sh
cloudflared tunnel --config /path/to/config.yml run my-tunnel
```

Can also specify by UUID:
```sh
cloudflared tunnel run <UUID>
```

## Config File Location and Search Order

On Linux, cloudflared looks for config in this order:
1. `~/.cloudflared/config.yml`
2. `/etc/cloudflared/config.yml`
3. `/usr/local/etc/cloudflared/config.yml`

## Example config.yml

```yaml
tunnel: <TUNNEL-UUID>
credentials-file: /home/user/.cloudflared/<TUNNEL-UUID>.json

ingress:
  - hostname: tunnel.example.com
    service: http://localhost:8080
  - hostname: api.example.com
    service: http://localhost:3000
  # Catch-all required:
  - service: http_status:404

# Optional logging config:
loglevel: info
logfile: /var/log/cloudflared/tunnel.log
```

## Credentials File Location

Generated during `cloudflared tunnel create`:
- Default path: `~/.cloudflared/<TUNNEL-UUID>.json`
- Must be readable by the cloudflared process.
- Contains the tunnel secret; treat as sensitive.

## Getting the Tunnel URL / Hostname

For named tunnels, there is **no auto-generated URL printed on startup**. The public hostname is whatever you configured via `cloudflared tunnel route dns`.

To look up configured routes:
```sh
# List all tunnels and their IDs
cloudflared tunnel list

# Get details for a specific tunnel
cloudflared tunnel info my-tunnel
```

The tunnel's internal Cloudflare address is always:
```
<TUNNEL-UUID>.cfargotunnel.com
```
Your configured DNS CNAME records point to this address.

## Named Tunnel Startup Log Output

Unlike quick tunnels, named tunnels do not print a URL box. The startup sequence looks like:

```
2024-12-05T10:30:15Z INF Starting tunnel tunnelID=a7b3c4d5-e6f7-8901-2345-6789abcdef01
2024-12-05T10:30:16Z INF Connection registered connIndex=0 location=IAD
2024-12-05T10:30:17Z INF Connection registered connIndex=1 location=ATL
2024-12-05T10:30:18Z INF Connection registered connIndex=2 location=ORD
2024-12-05T10:30:19Z INF Connection registered connIndex=3 location=DFW
```

When all 4 `Connection registered` lines appear, the tunnel is fully operational and serving traffic to the configured hostname(s).

All output is on **stderr**.

## Running with Token (No Config File)

Alternatively, named tunnels can be run with a token (useful for containers/CI):
```sh
cloudflared tunnel run --token <TOKEN>
```

## Sources

- https://developers.cloudflare.com/cloudflare-one/networks/connectors/cloudflare-tunnel/do-more-with-tunnels/local-management/create-local-tunnel/
- https://developers.cloudflare.com/cloudflare-one/networks/connectors/cloudflare-tunnel/do-more-with-tunnels/local-management/configuration-file/
- https://learnaws.io/blog/cloudflare-tunnel
