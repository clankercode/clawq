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
import sqlite3
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


PUBLICATION_PAUSE_SOURCE = r"""
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>
static void trace(const char *stage) {
    const char *path = getenv("B829_PID_PAUSE_TRACE");
    if (!path) return;
    int fd = syscall(SYS_openat, AT_FDCWD, path, O_WRONLY|O_CREAT|O_APPEND, 0600);
    if (fd >= 0) { dprintf(fd, "%ld %s\n", (long)getpid(), stage); close(fd); }
}
static void pause_pid(const char *path) {
    size_t n = strlen(path);
    const char *suffix = "/daemon.pid.new";
    size_t m = strlen(suffix);
    if (n >= m && strcmp(path + n - m, suffix) == 0) {
        trace("begin");
        usleep(500000);
        trace("end");
    }
}
int open(const char *path, int flags, ...) {
    static int (*original)(const char *, int, ...) = 0;
    if (!original) original = dlsym(RTLD_NEXT, "open");
    mode_t mode = 0;
    if (flags & O_CREAT) { va_list ap; va_start(ap, flags); mode = va_arg(ap, int); va_end(ap); }
    pause_pid(path);
    return original(path, flags, mode);
}
int open64(const char *path, int flags, ...) {
    static int (*original)(const char *, int, ...) = 0;
    if (!original) original = dlsym(RTLD_NEXT, "open64");
    mode_t mode = 0;
    if (flags & O_CREAT) { va_list ap; va_start(ap, flags); mode = va_arg(ap, int); va_end(ap); }
    pause_pid(path);
    return original(path, flags, mode);
}
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--publication-pause-ms", type=int, choices=(0, 500), default=0,
                        help="Pause the real scratch daemon before PID publication using a test-only preload")
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
    if args.publication_pause_ms:
        unset_keys = [key for key in unset_keys if key not in {"LD_PRELOAD", "B829_PID_PAUSE_TRACE"}]
    home_quoted = json.dumps("HOME=" + str(scratch)).replace("%", "%%")
    isolation = ("\n[Service]\nEnvironment=" + home_quoted
                 + "\nEnvironment=PATH=/usr/bin:/bin\nUnsetEnvironment="
                 + " ".join(unset_keys) + "\n")
    pause_dir = None
    pause_trace = None
    if args.publication_pause_ms:
        # LD_PRELOAD splits paths on whitespace: keep this helper outside the
        # fixture's deliberately awkward executable/data paths. No app records
        # are forged; the hook delays only the real daemon.pid.new open.
        pause_dir = Path(tempfile.mkdtemp(prefix="clawq-b829-publication-", dir="/tmp"))
        pause_source = pause_dir / "pause.c"
        pause_library = pause_dir / "pause.so"
        pause_trace = pause_dir / "trace"
        pause_source.write_text(PUBLICATION_PAUSE_SOURCE)
        try:
            run(["cc", "-shared", "-fPIC", "-o", str(pause_library), str(pause_source), "-ldl"])
        except BaseException:
            shutil.rmtree(pause_dir)
            shutil.rmtree(scratch)
            raise
        for value in ("LD_PRELOAD=" + str(pause_library), "B829_PID_PAUSE_TRACE=" + str(pause_trace)):
            isolation += "Environment=" + json.dumps(value).replace("%", "%%") + "\n"
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

    def logging_and_ec(pid):
        # A real daemon reload error must enter the existing EC scanner. Empty
        # model lists make its resulting diagnosis_failed report local-only.
        config_path = data / "config.json"
        before = (data / "daemon.log").stat().st_size
        try:
            config_path.write_text("{invalid scratch reload")
            os.kill(pid, signal.SIGHUP)
            wait_for("daemon error appended to daemon.log", lambda:
                     "Config reload failed" in (data / "daemon.log").read_text()[before:])

            def scanned():
                try:
                    with sqlite3.connect(data / "memory.db") as db:
                        row = db.execute("SELECT count(*) FROM ec_reports WHERE "
                                         "error_context LIKE '%Config reload failed%' "
                                         "AND status = 'diagnosis_failed'").fetchone()
                    return row[0] > 0
                except sqlite3.OperationalError:
                    return False
            wait_for("EC scanner consumes daemon-generated error without provider calls", scanned)
        finally:
            config_path.write_text(json.dumps(config))
        print("PASS foreground daemon.log append and real EC scanner report with no diagnosis models", flush=True)

    def startup_failure_cases():
        config_path = data / "config.json"
        original = json.dumps(config)

        def failure_pair(bad_config, evidence):
            for action in ("start", "restart"):
                cli("stop")
                config_path.write_text(original)
                if action == "restart":
                    cli("start")
                    ready(expected=int(property_value("MainPID")))
                config_path.write_text(json.dumps(bad_config))
                try:
                    result = cli(action, check=False)
                    assert result.returncode != 0, (action, result.stdout)
                    assert "under systemd supervision" not in result.stdout, result.stdout
                    assert "failed to become ready" in result.stdout, result.stdout
                    assert evidence in result.stdout, result.stdout
                finally:
                    ctl("stop")
                    config_path.write_text(original)
                assert property_value("MainPID") == "0"

        invalid = json.loads(original)
        invalid["gateway"].update(host="0.0.0.0", auth_token=None)
        failure_pair(invalid, "Refusing to bind")
        print("PASS invalid-config start and restart return nonzero with startup rejection detail", flush=True)
        with socket.socket() as blocker:
            blocker.bind(("127.0.0.1", 0))
            blocker.listen()
            occupied = json.loads(original)
            occupied["gateway"]["port"] = blocker.getsockname()[1]
            failure_pair(occupied, "Gateway server error")
        print("PASS occupied-gateway start and restart return nonzero; foreign listener is not readiness", flush=True)
        cli("start")
        ready(expected=int(property_value("MainPID")))

    def controlled_publication_cases():
        for action in ("install", "start", "restart"):
            if action == "start":
                cli("stop")
            before = len(pause_trace.read_text().splitlines())
            started_at = time.monotonic()
            result = cli(action)
            elapsed = time.monotonic() - started_at
            pid = int(property_value("MainPID"))
            current = wait_for("controlled publication daemon and EC startup", lambda: ready(expected=pid))
            assert current[0] == pid and elapsed >= 0.5, (action, elapsed)
            markers = pause_trace.read_text().splitlines()[before:]
            assert f"{pid} begin" in markers and f"{pid} end" in markers, markers
            assert f"{pid} begin" in markers[:markers.index(f"{pid} end")]
            assert result.returncode == 0
            print(f"PASS controlled 500ms publication pause {action}: exit=0 elapsed={elapsed:.3f}s "
                  f"MainPID={pid} EC={current[1]}; real PID/metadata/locks/heartbeat and signed HTTP200", flush=True)

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
        logging_and_ec(pid)

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
        assert "SIGUSR1 received" in (data / "daemon.log").read_text()
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
        # The installed inactive unit does not own a separately launched agent.
        with (scratch / "manual-agent.log").open("w") as output:
            manual = subprocess.Popen([str(binary), "agent"], env=client_env,
                                      stdout=output, stderr=subprocess.STDOUT)
        manual_pid, manual_ec = wait_for("manual agent with installed inactive unit",
                                         lambda: ready(expected=manual.pid))
        for action in ("start", "restart"):
            result = cli(action, check=False)
            assert result.returncode != 0 and "service install" in result.stdout, result.stdout
            assert alive(manual_pid) and alive(manual_ec)
            assert property_value("MainPID") == "0"
        print("PASS installed inactive unit start/restart refuse healthy manual owner and direct explicit adoption", flush=True)
        cli("stop")
        manual.wait(timeout=10)
        assert not alive(manual_pid) and not alive(manual_ec)
        assert property_value("MainPID") == "0" and not (data / "daemon.pid").exists()
        print("PASS installed inactive unit stop terminates actual manual daemon and EC", flush=True)
        cli("start")
        wait_for("managed start", lambda: ready(expected=int(property_value("MainPID"))))
        if pause_trace is not None:
            controlled_publication_cases()
        startup_failure_cases()
        cli("uninstall")
        assert not generated.exists() and not unit_path.exists()
        bad_env = dict(client_env, B829_FAIL_COMMAND="daemon-reload")
        failure = cli("install", env=bad_env, check=False)
        assert failure.returncode != 0, failure.stdout
        assert "injected user bus failure" in failure.stdout, failure.stdout
        print("PASS uninstall and actionable nonzero install failure", flush=True)
        print("PASS B829: original 7 scenario groups plus 5 review regression groups; isolated scratch state only", flush=True)
        if pause_trace is not None:
            print("PASS B829 round2: controlled publication pause on install/start/restart", flush=True)
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
            if pause_dir is not None:
                shutil.rmtree(pause_dir)
        else:
            print(f"Scratch evidence retained at {scratch}", flush=True)
            if pause_dir is not None:
                print(f"Publication helper evidence retained at {pause_dir}", flush=True)


if __name__ == "__main__":
    main()
