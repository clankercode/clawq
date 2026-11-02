#!/usr/bin/env python3
"""Opt-in B829 integration regression; run only on the x-p35 build box.

Uses an empty scratch HOME/config/DB, a unique real user unit, and no provider
credentials. The systemctl shim maps the installer's fixed clawq unit name to
that scratch unit. It never addresses the user's clawq.service or daemon.
"""

import argparse
import fcntl
import hashlib
import hmac
import json
import os
from pathlib import Path
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import uuid


def run(args, *, env=None, check=True):
    result = subprocess.run(args, env=env, text=True, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, timeout=60)
    if check and result.returncode:
        raise AssertionError(f"{args!r}: exit {result.returncode}\n{result.stdout}")
    return result


def wait_for(label, predicate, timeout=30):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            result = predicate()
            if result:
                return result
        except (OSError, ValueError, AssertionError):
            pass
        time.sleep(0.1)
    raise AssertionError(f"timeout: {label}")


def alive(pid):
    try:
        return Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()[0] != "Z"
    except FileNotFoundError:
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True)
    args = parser.parse_args()
    assert socket.gethostname() == "x-p35", "This regression runs only on x-p35"
    real_systemctl = shutil.which("systemctl")
    assert real_systemctl
    unit = f"clawq-b829-{uuid.uuid4().hex[:12]}.service"
    unit_path = Path.home() / ".config/systemd/user" / unit
    assert not unit_path.exists()
    scratch = Path(tempfile.mkdtemp(prefix="clawq B829 % $ "))
    data = scratch / "data % $"
    data.mkdir()
    workspace = scratch / "workspace"
    workspace.mkdir()
    shim_dir = scratch / "bin"
    shim_dir.mkdir()
    binary = shim_dir / "clawq quoted % $"
    shutil.copy2(args.binary.resolve(), binary)
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    config = {
        "workspace": str(workspace),
        "gateway": {"host": "127.0.0.1", "port": port,
                    "require_pairing": False, "auth_token": "b829-local-only"},
        "channels": {"github": {"auth": {"type": "pat", "token": "b829-dummy"},
                                "repos": [{"name": "scratch/b829", "webhook_secret": "scratch-secret",
                                           "webhook_path": "/github/webhook/b829", "react_to": []}]}},
        "tunnel": {"enabled": False},
        "heartbeat": {"enabled": False},
        "error_watcher": {"enabled": True, "scan_interval_s": 0.2,
                          "primary_models": [], "fallback_models": [],
                          "ignore_patterns": [".*"], "auto_fix_enabled": False},
    }
    (data / "config.json").write_text(json.dumps(config))
    client_env = {"HOME": str(scratch), "CLAWQ_HOME": str(data),
                  "PATH": f"{shim_dir}:/usr/bin:/bin",
                  "XDG_RUNTIME_DIR": f"/run/user/{os.getuid()}",
                  "DBUS_SESSION_BUS_ADDRESS": f"unix:path=/run/user/{os.getuid()}/bus"}
    # Strip manager credentials from the daemon and its EC child. Values are
    # never printed or written to disk.
    manager_env = run([real_systemctl, "--user", "show-environment"]).stdout
    unset_keys = [line.split("=", 1)[0] for line in manager_env.splitlines()
                  if "=" in line and line.split("=", 1)[0] not in
                  {"HOME", "PATH", "CLAWQ_HOME", "OCAMLRUNPARAM"}]
    home_quoted = json.dumps("HOME=" + str(scratch)).replace("%", "%%")
    isolation = ("\n[Service]\nEnvironment=" + home_quoted
                 + "\nEnvironment=PATH=/usr/bin:/bin\nUnsetEnvironment="
                 + " ".join(unset_keys) + "\n")
    generated = scratch / ".config/systemd/user/clawq.service"
    shim = f'''#!/usr/bin/python3
import os, pathlib, subprocess, sys
real = {real_systemctl!r}
unit = {unit!r}
generated = pathlib.Path({str(generated)!r})
target = pathlib.Path({str(unit_path)!r})
args = sys.argv[1:]
if args[:1] != ["--user"] or any(a == "clawq.service" for a in args):
    sys.exit("unexpected systemctl invocation")
if os.environ.get("B829_FAIL_COMMAND") in args:
    sys.exit("injected user bus failure")
if "daemon-reload" in args:
    if generated.exists():
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(generated.read_text() + {isolation!r})
    elif target.exists():
        target.unlink()
args = [unit if a == "clawq" else a for a in args]
sys.exit(subprocess.call([real] + args))
'''
    (shim_dir / "systemctl").write_text(shim)
    (shim_dir / "systemctl").chmod(0o755)
    manual = None

    def cli(*command, check=True, env=None):
        return run([str(binary), "service", *command], env=env or client_env, check=check)

    def ctl(*command, check=True):
        return run([real_systemctl, "--user", *command, unit], check=check)

    def property_value(name):
        return ctl("show", "-p", name, "--value").stdout.strip()

    def read_pid(name="daemon.pid"):
        return int((data / name).read_text().strip())

    def check_lock(name):
        with (data / name).open("r+") as lock:
            try:
                fcntl.lockf(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                return
            raise AssertionError(f"{name} is not held")

    def webhook():
        payload = '{"zen":"scratch supervisor probe"}'
        signature = hmac.new(b"scratch-secret", payload.encode(), hashlib.sha256).hexdigest()
        result = run(["curl", "--silent", "--show-error", "--max-time", "2",
                      "--write-out", "\n%{http_code}", "-H", "X-GitHub-Event: ping",
                      "-H", "Content-Type: application/json",
                      "-H", "X-Hub-Signature-256: sha256=" + signature,
                      "--data", payload, f"http://127.0.0.1:{port}/github/webhook/b829"])
        assert result.stdout.endswith("\n200"), result.stdout
        assert '"accepted"' in result.stdout, result.stdout
        return True

    def ready(expected=None, previous=None):
        pid = read_pid()
        assert pid > 1 and alive(pid) and pid != previous
        if expected is not None:
            assert pid == expected
        ec = read_pid("ec_process.pid")
        assert ec > 1 and alive(ec)
        assert int(Path(f"/proc/{ec}/stat").read_text().rsplit(")", 1)[1].split()[1]) == pid
        assert int((data / "daemon.lock").read_text().strip()) == pid
        assert int((data / "ec_process.lock").read_text().strip()) == ec
        heartbeat = float((data / "heartbeat").read_text().split()[0])
        assert time.time() - heartbeat < 30
        check_lock("daemon.lock")
        check_lock("ec_process.lock")
        webhook()
        return pid, ec

    try:
        with (scratch / "manual.log").open("w") as output:
            manual = subprocess.Popen([str(binary), "service", "run"], env=client_env,
                                      stdout=output, stderr=subprocess.STDOUT)
        old_pid, old_ec = wait_for("manual foreground startup", lambda: ready(expected=manual.pid))
        duplicate = cli("run", check=False)
        assert duplicate.returncode != 0 and "lock is held" in duplicate.stdout, duplicate.stdout
        assert read_pid() == old_pid
        print(f"PASS foreground ownership and duplicate refusal: daemon={old_pid} EC={old_ec}", flush=True)

        template = cli("systemd-unit").stdout
        installed = cli("install")
        assert generated.read_text() == template
        assert "started under systemd supervision" in installed.stdout, installed.stdout
        manual.wait(timeout=10)
        pid, ec = wait_for("adopted supervised daemon", lambda: ready(expected=int(property_value("MainPID"))))
        assert pid != old_pid and not alive(old_ec)
        assert property_value("Restart") == "on-failure"
        assert property_value("KillMode") == "control-group"
        assert property_value("WatchdogUSec") == "0"
        assert int(property_value("MemoryHigh")) == 1024**3
        assert int(property_value("MemoryMax")) == 1536 * 1024**2
        assert ctl("is-enabled").stdout.strip() == "enabled"
        assert unit in Path(f"/proc/{ec}/cgroup").read_text()
        print(f"PASS install adopts existing daemon: MainPID={pid} EC={ec}, memory guards intact", flush=True)

        killed_at = time.monotonic()
        heartbeat_before = (data / "heartbeat").read_text()
        assert str(binary).encode() in Path(f"/proc/{pid}/cmdline").read_bytes()
        os.kill(pid, signal.SIGKILL)
        new_pid, new_ec = wait_for("SIGKILL recovery", lambda: ready(previous=pid), timeout=29)
        elapsed = time.monotonic() - killed_at
        assert new_pid == int(property_value("MainPID"))
        assert new_ec != ec and not alive(ec)
        assert (data / "heartbeat").read_text() != heartbeat_before
        assert int(property_value("NRestarts")) >= 1
        print(f"PASS SIGKILL recovery in {elapsed:.3f}s: daemon={new_pid} EC={new_ec}; lock/pid/heartbeat and signed curl webhook HTTP 200", flush=True)

        ctl("reload")
        def reloaded():
            current = ready(expected=new_pid)
            assert current[1] != new_ec
            return current
        pid, ec = wait_for("graceful same-PID reload", reloaded)
        print(f"PASS graceful reload: same MainPID={pid}, replacement EC={ec}", flush=True)

        cli("restart")
        pid, ec = wait_for("managed restart", lambda: ready(previous=pid))
        cli("stop")
        assert property_value("MainPID") == "0"
        assert not (data / "daemon.pid").exists()
        assert not alive(pid) and not alive(ec)
        restarts = property_value("NRestarts")
        time.sleep(6)
        assert property_value("MainPID") == "0"
        assert property_value("NRestarts") == restarts
        print("PASS managed restart and clean stop: no automatic resurrection", flush=True)
        cli("start")
        wait_for("managed start", lambda: ready(expected=int(property_value("MainPID"))))
        cli("uninstall")
        assert not generated.exists() and not unit_path.exists()
        bad_env = dict(client_env, B829_FAIL_COMMAND="daemon-reload")
        failure = cli("install", env=bad_env, check=False)
        assert failure.returncode != 0, failure.stdout
        assert "injected user bus failure" in failure.stdout, failure.stdout
        print("PASS uninstall and actionable nonzero install failure", flush=True)
        print("PASS B829: 7 integration scenarios, isolated scratch state only", flush=True)
    except BaseException:
        print(ctl("status", "--no-pager", check=False).stdout, flush=True)
        for name in ("daemon.log", "ec_process.log"):
            path = data / name
            if path.exists():
                print(f"{name} tail:\n" + path.read_text(errors="replace")[-5000:], flush=True)
        raise
    finally:
        ctl("stop", check=False)
        ctl("disable", check=False)
        unit_path.unlink(missing_ok=True)
        run([real_systemctl, "--user", "daemon-reload"], check=False)
        ctl("reset-failed", check=False)
        if manual is not None and manual.poll() is None:
            manual.terminate()
            try:
                manual.wait(timeout=10)
            except subprocess.TimeoutExpired:
                manual.kill()
                manual.wait()
        # Keep scratch evidence on failure; successful runs remove all fixtures.
        if not sys.exc_info()[0]:
            shutil.rmtree(scratch)
        else:
            print(f"Scratch evidence retained at {scratch}", flush=True)


if __name__ == "__main__":
    main()
