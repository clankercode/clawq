# Daemon Supervision and Recovery (B823, B829)

A Linux user service supervises the daemon process and recovers from crashes,
OOM kills, and SIGKILL. The liveness heartbeat also reveals a daemon whose
process is alive but whose event loop has stopped progressing.

## Linux service supervision

Install from the **stable installed binary** you want systemd to run:

```sh
clawq service install
systemctl --user show clawq -p MainPID -p Restart -p ActiveState
clawq service status
journalctl --user -u clawq -f
```

Installation writes `~/.config/systemd/user/clawq.service`, reloads the user
manager, enables autostart, and starts the service immediately. If a manually
started daemon exists for the current data directory, installation stops it
before starting the supervised instance. If systemd already owns the daemon,
installation restarts the unit to apply the new launch contract. A stop,
reload, enable, or start failure is reported with the failed command and its
output; installation does not claim success. Success requires matching manager
and daemon PIDs, a lock holder, and a fresh heartbeat.

`clawq service start`, `stop`, and `restart` use the installed unit when its
`CLAWQ_HOME` binding matches the current data directory. An explicit managed
stop stays stopped. `clawq service signal-restart` and `systemctl --user reload
clawq` perform the daemon's graceful in-place restart and retain the PID.
`clawq service uninstall` stops and disables the unit before removing it.

The generated unit uses `Type=exec` and runs `clawq service run` in the
foreground. That entrypoint acquires `daemon.lock`, writes `daemon.pid` and
its identity metadata, propagates startup failures as process failures, and
cleans up on ordinary exit. A second foreground instance fails while the lock
is held. `Restart=on-failure` and `RestartSec=5` recover unexpected exits;
`KillMode=control-group` removes the old EC process and other children before
restarting. The new daemon starts its own EC watcher when enabled. The unit
orders startup after `network-online.target`; a user manager's ordering does
not guarantee that external networks are reachable, so connectors still own
connection retries.

There is no `WatchdogSec`: the daemon does not implement `sd_notify`, so an
armed systemd watchdog would kill a healthy process after the watchdog timeout.
The heartbeat is an operator liveness signal, and a hung managed daemon needs
`clawq service restart` or an external heartbeat monitor. The unit preserves
B830's `OCAMLRUNPARAM=o=60`, `MemoryHigh=1G`, `MemoryMax=1536M`, and
`OOMPolicy=stop`. Under earlyoom or kernel OOM pressure, a killed daemon now
recovers automatically; the existing bounded webhook queue/worker limits
remain in force. These limits bound damage, and do not prove that a workload
fits within the available memory.

The unit captures the current data directory through `CLAWQ_HOME` and quotes
paths and spaces, escapes systemd percent specifiers, and preserves literal
dollars in the executable name. Keep the binary
at a durable executable path; generating a unit from a temporary build or
worktree pins it to that path. Maintain any required runner/tool PATH in the
user manager environment or a unit drop-in. Run `loginctl enable-linger "$USER"`
if the service must start at boot and stay available after logout; otherwise
the enabled user unit starts with the login session.

Deployment requires the rebuilt stable binary, the install/adoption step, a
working user systemd bus, and optional linger. Installing/adopting a real daemon
interrupts it briefly. The B829 scratch regression does not deploy the worktree
binary for the real user or control the production daemon.

### Isolated recovery regression

On the x-p35 build box, after building the candidate binary:

```sh
python3 test/service_supervision_integration.py --binary _build/default/src/main.exe
```

This opt-in regression uses a unique `clawq-b829-*.service`, empty scratch
HOME/config/DB, a loopback port, fake GitHub credentials, and an EC watcher with
no diagnosis models. A narrow systemctl shim maps only the installer's `clawq`
unit argument to the scratch unit. It tests real install/adoption, foreground
lock exclusion, SIGKILL recovery within 30 seconds, EC replacement, fresh
heartbeat/PID/lock state, a signed curl webhook returning HTTP 200, graceful
reload, managed restart/stop/start/uninstall, and command failures. It removes
the scratch unit on exit and retains scratch logs if a check fails. Batch
build/test invocations must run through the coordinator's global gate.

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

- Unmanaged `clawq service start` — the pid-file gate replaces a hung holder
  (and cleans up its orphaned EC child) before forking the replacement. For an
  installed systemd unit, use `clawq service restart` to recover a hung daemon.
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
file + rename) and always reflect the live instance. Graceful in-place
restarts (`service signal-restart`, unit reload, self-update re-exec) retain the
PID. `service restart` stops and starts the daemon, giving
it a new PID.

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
  heartbeat file exists at all); use `clawq service restart` for a managed
  daemon.
- `heartbeat: no daemon` — no live daemon pid recorded.

## Relation to the B652 watchdog

The B652 agent-turn watchdog detects a *stuck conversation turn* inside the
daemon and pauses that session. It cannot help when the daemon's whole event
loop is blocked — that is what the heartbeat + takeover described here covers.
The two mechanisms are complementary and independent.
