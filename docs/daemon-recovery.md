# Daemon Hang Recovery (B823)

The daemon can hang with a live process but a dead event loop: every connector
stops, the UI listener refuses connections, and nothing recovers it because
`clawq service start` sees a live pid and refuses to start a replacement. This
page describes the liveness heartbeat and takeover mechanism that makes hangs
self-healing (whatever their root cause).

## Heartbeat

The daemon writes `~/.clawq/heartbeat` **from the main event loop** every at
most 30 seconds. The file is written atomically (temp file + rename) and
contains one line:

```
<unix_ts_seconds> <iso8601_utc>
```

for example `1750000000.500 2025-06-15T15:06:40.500Z`.

Because the writer runs on the daemon's Lwt loop, a fully blocked loop stops
the heartbeat while the process stays alive — exactly the "alive but dead"
state that used to be undetectable. The first beat is written immediately at
daemon startup, so a freshly started daemon is never mistaken for a hung one.

## Staleness threshold

A heartbeat is **stale** when it is older than the threshold (default
**300 s**). The threshold can be configured with the environment variable:

```
CLAWQ_HEARTBEAT_STALE_S=60
```

## Takeover

When a start path finds the daemon lock or `daemon.pid` held, it decides from
the heartbeat whether the holder may be replaced:

| Holder state                                      | Decision |
| ------------------------------------------------- | -------- |
| Heartbeat fresh (younger than threshold)          | **Refuse** — a healthy daemon is running; the message says so and points at `clawq service restart` / `service status`. |
| Heartbeat stale (holder alive, hung loop)         | **Take over** — SIGTERM the holder, wait up to 10 s, SIGKILL if needed, take the lock, log the takeover. |
| Holder pid gone (crash left lock/pid files)       | **Take over** — clean up stale state and take the lock; no manual `rm ~/.clawq/*.pid` needed. |
| No heartbeat file ever written (pre-B823 daemon)  | **Take over** — nothing proves the holder is alive-and-progressing. |
| EC process whose parent daemon is gone (orphan)   | **Take over** — the EC lock follows the daemon, not the OS process. |

Takeover is wired into:

- `clawq service start` — the pid-file gate replaces a hung holder (and cleans
  up the hung daemon's orphaned EC child) before forking the replacement.
- `clawq agent` — same decision when the daemon singleton lock
  (`~/.clawq/daemon.lock`) is held. The lock file records the holder pid so the
  decision never depends on `daemon.pid` being fresh.
- the Error Correction watcher process (`clawq ec-run --daemon-mode`) — its own
  lock (`~/.clawq/ec_process.lock`) is judged from the **holder pid itself**
  (zombie-aware liveness), not the daemon heartbeat: EC children never write
  heartbeats, so a fresh daemon heartbeat cannot vouch for a dying EC sibling.
  A live holder is never killed; a dead, zombie (crashed, unreaped), or
  orphaned holder is replaced. The daemon also reaps exited EC children
  (`waitpid` in the periodic health check) so a crash cannot linger as a
  zombie that blocks respawn, and a duplicate start is skipped while the
  recorded holder is alive (the spawn log line names the spawner).

Every takeover is logged to `~/.clawq/daemon.log` (and stderr) with a line
containing `takeover:` plus the reason, e.g.

```
[2025-06-15T15:12:03.400Z] takeover: holder pid 1448200 heartbeat stale (421s old >= threshold 300s); holder pid 1448200 terminated after SIGTERM
```

## Pid files

`~/.clawq/daemon.pid` (with its `daemon.pid.meta` identity file) and
`~/.clawq/ec_process.pid` are rewritten **atomically on every start** (temp
file + rename) and always reflect the live instance. In-place restarts
(`service restart`, `service signal-restart`, self-update re-exec) keep the
same pid, so the files stay accurate across them.

## `clawq service status`

Reports the heartbeat verdict for operators:

```
Service status:
  daemon: running (pid 1448200, uptime 2h 13m)
  heartbeat: 5s old — verdict: healthy (pid 1448200)
  lock holder: pid 1448200 (/home/user/.clawq/daemon.lock)
  ...
```

- `verdict: healthy` — holder alive, heartbeat fresh.
- `verdict: stale` — holder alive but heartbeat is past the threshold (or no
  heartbeat file exists at all); `clawq service start` will take over.
- `heartbeat: no daemon` — no live daemon pid recorded.

## Relation to the B652 watchdog

The B652 agent-turn watchdog detects a *stuck conversation turn* inside the
daemon and pauses that session. It cannot help when the daemon's whole event
loop is blocked — that is what the heartbeat + takeover described here covers.
The two mechanisms are complementary and independent.
