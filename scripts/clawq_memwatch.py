#!/usr/bin/env python3
"""Record Linux daemon memory evidence without changing the running daemon."""

import argparse
from collections import deque
import csv
import fcntl
import json
import os
from pathlib import Path
import time


FIELDS = (
    "ts", "pid", "start_ticks", "status", "rss_kb", "pss_kb", "swap_kb",
    "rss_swap_kb", "uptime_s", "heartbeat_age_s",
)
MAX_GAP = 180
FOURTEEN_DAYS = 14 * 86400


def read_rows(state_dir):
    path = state_dir / "samples.csv"
    if not path.exists():
        return
    with path.open() as stream:
        fcntl.flock(stream, fcntl.LOCK_SH)
        yield from csv.DictReader(stream)


def sample(clawq_home, state_dir, proc_root=Path("/proc"), now=None, ticks=None):
    now = int(time.time() if now is None else now)
    ticks = os.sysconf("SC_CLK_TCK") if ticks is None else ticks
    row = dict.fromkeys(FIELDS, "")
    row.update(ts=now, status="dead")
    try:
        pid = int((clawq_home / "daemon.pid").read_text().strip())
        if pid <= 0:
            raise ValueError("invalid daemon pid")
        row["pid"] = pid
        process = proc_root / str(pid)
        # The comm field may itself contain spaces and parentheses.
        stat_fields = (process / "stat").read_text().rsplit(")", 1)[1].split()
        row["start_ticks"] = int(stat_fields[19])
        uptime = float((proc_root / "uptime").read_text().split()[0])
        row["uptime_s"] = max(0, int(uptime - row["start_ticks"] / ticks))
        if stat_fields[0] == "Z":
            raise ProcessLookupError("daemon is a zombie")
        values = {}
        for line in (process / "smaps_rollup").read_text().splitlines():
            name, _, value = line.partition(":")
            if name in ("Rss", "Pss", "Swap"):
                values[name] = int(value.split()[0])
        for key in ("Rss", "Pss", "Swap"):
            row[key.lower() + "_kb"] = values[key]
        row["rss_swap_kb"] = row["rss_kb"] + row["swap_kb"]
        age = now - (clawq_home / "heartbeat").stat().st_mtime
        row["heartbeat_age_s"] = round(age, 1)
        row["status"] = "healthy" if -30 <= age <= 120 else "stale"
        if row["rss_swap_kb"] >= 512 * 1024:
            row["status"] = "memory_pressure"
    except (FileNotFoundError, ProcessLookupError):
        pass
    except (OSError, ValueError, IndexError, KeyError):
        # Permission errors and malformed proc data are missing evidence.
        row["status"] = "unreadable"

    state_dir.mkdir(parents=True, exist_ok=True)
    with (state_dir / "samples.csv").open("a+", newline="") as stream:
        fcntl.flock(stream, fcntl.LOCK_EX)
        end = stream.seek(0, os.SEEK_END)
        offset = max(0, end - 8192)
        stream.seek(offset)
        if offset:
            stream.readline()
        previous = deque(csv.DictReader(stream, fieldnames=FIELDS if offset else None), maxlen=5)
        if row["status"] == "healthy":
            generation = (str(row["pid"]), str(row["start_ticks"]))
            recent = [
                prior for prior in previous
                if prior["status"] == "healthy"
                and 0 < now - int(prior["ts"]) <= 300
                and (prior["pid"], prior["start_ticks"]) == generation
            ]
            if recent and row["rss_swap_kb"] - int(recent[0]["rss_swap_kb"]) > 100 * 1024:
                row["status"] = "memory_growth"
        stream.seek(0, os.SEEK_END)
        writer = csv.DictWriter(stream, fieldnames=FIELDS)
        if stream.tell() == 0:
            writer.writeheader()
        writer.writerow(row)
    return row


def report(state_dir, now=None):
    now = int(time.time() if now is None else now)
    start = previous_ts = None
    gaps = unhealthy = 0
    count, first_ts, last_row = 0, None, None
    for row in read_rows(state_dir):
        ts = int(row["ts"])
        count += 1
        if first_ts is None:
            first_ts = ts
        last_row = row
        if previous_ts is not None and not 0 < ts - previous_ts <= MAX_GAP:
            gaps += 1
            start = None
        if row["status"] != "healthy":
            unhealthy += 1
            start = None
        elif start is None:
            start = ts
        previous_ts = ts
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
        "observed_healthy_seconds": duration,
        "fourteen_days_observed": duration >= FOURTEEN_DAYS,
        "review_required": "Check OOM/kill journals and hourly mem_stats for leaks before closing B830; samples alone cannot prove their absence.",
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("sample", "report"))
    parser.add_argument("--clawq-home", type=Path, default=Path(os.environ.get("CLAWQ_HOME", str(Path.home() / ".clawq"))))
    parser.add_argument("--state-dir", type=Path, default=Path(os.environ.get("XDG_STATE_HOME", str(Path.home() / ".local/state"))) / "clawq/memwatch")
    parser.add_argument("--write-summary", action="store_true")
    args = parser.parse_args()
    if args.action == "sample":
        result = sample(args.clawq_home, args.state_dir)
    else:
        result = report(args.state_dir)
    output = json.dumps(result, sort_keys=True)
    if args.write_summary:
        args.state_dir.mkdir(parents=True, exist_ok=True)
        temporary = args.state_dir / "summary.json.tmp"
        temporary.write_text(output + "\n")
        temporary.replace(args.state_dir / "summary.json")
    print(output)


if __name__ == "__main__":
    main()
