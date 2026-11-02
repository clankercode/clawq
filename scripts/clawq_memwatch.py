#!/usr/bin/env python3
"""Record Linux daemon memory evidence without changing the running daemon."""

import argparse
from collections import deque
import csv
import fcntl
import hashlib
import io
import json
import math
import os
from pathlib import Path
import re
import tempfile
import time
import uuid


FIELDS = (
    "ts", "pid", "start_ticks", "status", "rss_kb", "pss_kb", "swap_kb",
    "rss_swap_kb", "uptime_s", "heartbeat_age_s",
    "boot_id", "image_id", "runtime_id",
)
MAX_GAP = 180
FOURTEEN_DAYS = 14 * 86400
IDENTITY_FIELDS = ("pid", "start_ticks", "boot_id", "image_id", "runtime_id")
INTEGER_FIELDS = ("ts", "pid", "start_ticks", "rss_kb", "pss_kb", "swap_kb",
                  "rss_swap_kb", "uptime_s")
STATUSES = {"healthy", "dead", "unreadable", "identity_unknown", "identity_changed",
            "stale", "memory_pressure", "memory_growth", "invalid_evidence"}


class IdentityUnknown(ValueError):
    """The PID cannot be tied to a known Clawq runtime."""


def file_identity(stat):
    return f"{stat.st_dev}:{stat.st_ino}:{stat.st_size}:{stat.st_mtime_ns}"


def runtime_identity(clawq_home, proc_root):
    pid = int((clawq_home / "daemon.pid").read_text().strip())
    if pid <= 0:
        raise IdentityUnknown("invalid pid")
    process = proc_root / str(pid)
    stat = (process / "stat").read_text()
    # The comm field may itself contain spaces and parentheses.
    fields = stat.rsplit(")", 1)[1].split()
    start = fields[19]
    if fields[0] == "Z":
        raise ProcessLookupError("daemon is a zombie")
    if not stat.startswith(f"{pid} (") or not start.isdecimal():
        raise IdentityUnknown("invalid process identity")
    try:
        # service.ml writes plain start ticks at daemonized startup. In-place
        # exec can retain this file, so also fingerprint its exec layout below.
        with (clawq_home / "daemon.pid.meta").open() as stream:
            expected = stream.read().strip()
            meta_identity = file_identity(os.fstat(stream.fileno()))
        if expected != start:
            raise IdentityUnknown("start ticks do not match daemon.pid.meta")
        boot = (proc_root / "sys/kernel/random/boot_id").read_text().strip()
        if str(uuid.UUID(boot)) != boot:
            raise IdentityUnknown("invalid boot identity")
        image = file_identity((process / "exe").stat())
        # Stable exec layout fields, excluding mutable brk/register/RSS values.
        layout = [fields[index] for index in (23, 24, 25, 42, 43, 44, 45, 46, 47, 48)]
        if not all(value.isdecimal() for value in layout) or int(layout[2]) == 0:
            raise IdentityUnknown("exec layout is unavailable")
    except (OSError, ValueError, IndexError) as error:
        raise IdentityUnknown(str(error)) from error
    runtime = hashlib.sha256((meta_identity + ":" + ":".join(layout)).encode()).hexdigest()
    return dict(pid=pid, start_ticks=int(start), boot_id=boot, image_id=image, runtime_id=runtime)


def generation(row):
    return tuple(str(row[key]) for key in IDENTITY_FIELDS)


def csv_values(raw):
    if not raw.endswith(b"\n"):
        raise ValueError("unterminated record")
    return next(csv.reader([raw.decode("utf-8")], strict=True))


def header_ok(raw):
    try:
        return tuple(csv_values(raw)) == FIELDS
    except (ValueError, csv.Error):
        return False


def parse_record(raw, valid_header=True):
    """One physical line per record; corruption cannot swallow later evidence."""
    try:
        if not valid_header:
            raise ValueError("unrecognized CSV header")
        values = csv_values(raw)
        if len(values) != len(FIELDS):
            raise ValueError("wrong field count")
        row = dict(zip(FIELDS, values))
        if row["status"] not in STATUSES:
            raise ValueError("unknown status")
        if row["status"] == "invalid_evidence":
            raise ValueError("recovered unterminated tail")
        for key in INTEGER_FIELDS:
            if row[key] != "":
                if not re.fullmatch(r"[0-9]+", row[key]):
                    raise ValueError(f"invalid {key}")
                row[key] = int(row[key])
        if row["ts"] == "" or row["ts"] <= 0:
            raise ValueError("missing timestamp")
        if row["heartbeat_age_s"] != "":
            row["heartbeat_age_s"] = float(row["heartbeat_age_s"])
            if not math.isfinite(row["heartbeat_age_s"]):
                raise ValueError("invalid heartbeat age")
        if row["status"] == "healthy":
            if any(row[key] == "" for key in FIELDS):
                raise ValueError("incomplete healthy record")
            if row["pid"] <= 0 or row["start_ticks"] <= 0:
                raise ValueError("invalid process identity")
            if str(uuid.UUID(row["boot_id"])) != row["boot_id"]:
                raise ValueError("invalid boot identity")
            if not re.fullmatch(r"[0-9]+:[0-9]+:[0-9]+:[0-9]+", row["image_id"]):
                raise ValueError("invalid image identity")
            if not re.fullmatch(r"[0-9a-f]{64}", row["runtime_id"]):
                raise ValueError("invalid runtime identity")
            if row["rss_swap_kb"] != row["rss_kb"] + row["swap_kb"]:
                raise ValueError("inconsistent memory metrics")
            if not -30 <= row["heartbeat_age_s"] <= 120 or row["rss_swap_kb"] >= 512 * 1024:
                raise ValueError("unhealthy metrics labelled healthy")
        return row
    except (ValueError, csv.Error) as error:
        # The bytes stay in samples.csv. Never echo potentially corrupt contents
        # into the summary; expose a bounded diagnostic instead.
        return {"status": "invalid_evidence", "error": str(error)[:160]}


def read_rows(state_dir):
    path = state_dir / "samples.csv"
    if not path.exists():
        return
    with path.open("rb") as stream:
        fcntl.flock(stream, fcntl.LOCK_SH)
        valid_header = header_ok(stream.readline())
        if not valid_header:
            yield {"status": "invalid_evidence", "error": "unrecognized CSV header"}
        for raw in stream:
            yield parse_record(raw, valid_header)


def sample(clawq_home, state_dir, proc_root=Path("/proc"), now=None, ticks=None):
    now = int(time.time() if now is None else now)
    ticks = os.sysconf("SC_CLK_TCK") if ticks is None else ticks
    row = dict.fromkeys(FIELDS, "")
    row.update(ts=now, status="dead")
    try:
        identity = runtime_identity(clawq_home, proc_root)
        row.update(identity)
        process = proc_root / str(row["pid"])
        uptime = float((proc_root / "uptime").read_text().split()[0])
        row["uptime_s"] = max(0, int(uptime - row["start_ticks"] / ticks))
        values = {}
        for line in (process / "smaps_rollup").read_text().splitlines():
            name, _, value = line.partition(":")
            if name in ("Rss", "Pss", "Swap"):
                values[name] = int(value.split()[0])
                if values[name] < 0 or value.split()[1:] != ["kB"]:
                    raise ValueError("invalid memory metric")
        for key in ("Rss", "Pss", "Swap"):
            row[key.lower() + "_kb"] = values[key]
        row["rss_swap_kb"] = row["rss_kb"] + row["swap_kb"]
        age = now - (clawq_home / "heartbeat").stat().st_mtime
        row["heartbeat_age_s"] = round(age, 1)
        row["status"] = "healthy" if -30 <= age <= 120 else "stale"
        if row["rss_swap_kb"] >= 512 * 1024:
            row["status"] = "memory_pressure"
        try:
            if runtime_identity(clawq_home, proc_root) != identity:
                row["status"] = "identity_changed"
        except (OSError, ValueError, IndexError):
            row["status"] = "identity_changed"
    except IdentityUnknown:
        row["status"] = "identity_unknown"
    except (FileNotFoundError, ProcessLookupError):
        pass
    except (OSError, ValueError, IndexError, KeyError):
        # Permission errors and malformed proc data are missing evidence.
        row["status"] = "unreadable"

    state_dir.mkdir(parents=True, exist_ok=True)
    with (state_dir / "samples.csv").open("a+b") as stream:
        fcntl.flock(stream, fcntl.LOCK_EX)
        stream.seek(0)
        valid_header = header_ok(stream.readline())
        end = stream.seek(0, os.SEEK_END)
        offset = max(0, end - 8192)
        stream.seek(offset)
        if offset:
            stream.readline()
        else:
            stream.readline()  # header
        previous = deque((parse_record(raw, valid_header) for raw in stream), maxlen=5)
        if row["status"] == "healthy":
            recent = []
            next_ts = now
            for prior in reversed(previous):
                if (prior["status"] != "healthy" or generation(prior) != generation(row)
                        or not 0 < next_ts - prior["ts"] <= MAX_GAP
                        or now - prior["ts"] > 300):
                    break
                recent.append(prior)
                next_ts = prior["ts"]
            if recent and row["rss_swap_kb"] - recent[-1]["rss_swap_kb"] > 100 * 1024:
                row["status"] = "memory_growth"
        # Do not join a crash-truncated row to the next sample or retroactively
        # bless a complete-looking row that lacked its terminating newline.
        recovered_tail = False
        if end:
            stream.seek(-1, os.SEEK_END)
            recovered_tail = stream.read(1) != b"\n"
        stream.seek(0, os.SEEK_END)
        output = io.StringIO(newline="")
        writer = csv.DictWriter(output, fieldnames=FIELDS)
        if end == 0:
            writer.writeheader()
        elif recovered_tail:
            output.write("\n")
            writer.writerow({"ts": now, "status": "invalid_evidence"})
        writer.writerow(row)
        stream.write(output.getvalue().encode("utf-8"))
    return row


def report(state_dir, now=None):
    now = int(time.time() if now is None else now)
    start = previous_ts = previous_identity = None
    gaps = unhealthy = 0
    invalid = boundaries = 0
    last_boundary = None
    errors = []
    count, first_ts, last_row = 0, None, None
    for row in read_rows(state_dir):
        count += 1
        last_row = row
        if row["status"] == "invalid_evidence":
            invalid += 1
            if len(errors) < 20:
                errors.append({"record": count, "error": row["error"]})
            start = previous_ts = previous_identity = None
            continue
        ts = row["ts"]
        if first_ts is None:
            first_ts = ts
        if previous_ts is not None and not 0 < ts - previous_ts <= MAX_GAP:
            gaps += 1
            start = None
        identity = generation(row)
        if previous_identity is not None and identity != previous_identity:
            boundaries += 1
            last_boundary = ts
            start = None
        if row["status"] != "healthy":
            unhealthy += 1
            start = None
        elif start is None:
            start = ts
        previous_ts = ts
        previous_identity = identity
    duration = 0 if start is None else previous_ts - start
    fresh = previous_ts is not None and 0 <= now - previous_ts <= MAX_GAP
    if not fresh:
        duration = 0
    return {
        "sample_count": count,
        "first_sample_ts": first_ts,
        "last_sample": last_row,
        "sampling_current": fresh,
        "observation_gaps": gaps,
        "unhealthy_samples": unhealthy,
        "runtime_boundaries": boundaries,
        "last_runtime_boundary_ts": last_boundary,
        "invalid_records": invalid,
        "evidence_complete": count > 0 and invalid == 0,
        "evidence_errors": errors,
        "observed_healthy_seconds": duration,
        "fourteen_days_observed": duration >= FOURTEEN_DAYS and invalid == 0,
        "review_required": "B830 remains OPEN: review OOM/kill journals and hourly mem_stats. Any diagnostic/manual restart exception requires explicit reviewed evidence, separate from automatic duration credit.",
    }


def publish_summary(state_dir):
    state_dir.mkdir(parents=True, exist_ok=True)
    # Lock BEFORE taking a CSV snapshot and keep it through publication, so a
    # delayed writer cannot replace a newer report with its older snapshot.
    with (state_dir / "summary.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        result = report(state_dir)
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(mode="w", dir=state_dir, prefix=".summary-",
                                             suffix=".tmp", delete=False) as stream:
                temporary = Path(stream.name)
                stream.write(json.dumps(result, sort_keys=True) + "\n")
                stream.flush()
                os.fsync(stream.fileno())
            temporary.replace(state_dir / "summary.json")
        finally:
            if temporary is not None:
                temporary.unlink(missing_ok=True)
        return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("sample", "report"))
    parser.add_argument("--clawq-home", type=Path, default=Path(os.environ.get("CLAWQ_HOME", str(Path.home() / ".clawq"))))
    parser.add_argument("--state-dir", type=Path, default=Path(os.environ.get("XDG_STATE_HOME", str(Path.home() / ".local/state"))) / "clawq/memwatch")
    parser.add_argument("--write-summary", action="store_true")
    args = parser.parse_args()
    if args.write_summary and args.action != "report":
        parser.error("--write-summary requires report")
    if args.action == "sample":
        result = sample(args.clawq_home, args.state_dir)
    elif args.write_summary:
        result = publish_summary(args.state_dir)
    else:
        result = report(args.state_dir)
    output = json.dumps(result, sort_keys=True)
    print(output)


if __name__ == "__main__":
    main()
