"""Memory-watch evidence must distinguish observation from elapsed time."""

import csv
import contextlib
import importlib.util
import io
import json
import multiprocessing
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / "clawq_memwatch.py"
spec = importlib.util.spec_from_file_location("clawq_memwatch", SCRIPT)
watch = importlib.util.module_from_spec(spec)
spec.loader.exec_module(watch)


BOOT_ID = "07a010e1-411f-4d20-9c9e-123456789abc"


def summary_writer(state, captured, release, started, results, pause):
    """Pause an older snapshot while another CLI writer tries to publish."""
    original = watch.report

    def snapshot(*args, **kwargs):
        result = original(*args, **kwargs)
        captured.set()
        if pause and not release.wait(10):
            raise RuntimeError("snapshot release timed out")
        return result

    started.set()
    try:
        with mock.patch.object(watch, "report", snapshot), mock.patch.object(
            sys, "argv", [str(SCRIPT), "report", "--state-dir", str(state), "--write-summary"]
        ), contextlib.redirect_stdout(io.StringIO()) as output:
            watch.main()
        results.put((None, json.loads(output.getvalue())))
    except Exception as error:
        results.put((repr(error), None))


class MemoryWatchTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.home = self.root / "clawq"
        self.state = self.root / "state"
        self.proc = self.root / "proc"
        self.home.mkdir()
        self.proc.mkdir()

    def daemon(self, now=1000, rss=24000, start=10000, pid=42):
        (self.home / "daemon.pid").write_text(f"{pid}\n")
        meta = self.home / "daemon.pid.meta"
        if not meta.exists() or meta.read_text() != str(start):
            meta.write_text(str(start))
        boot = self.proc / "sys/kernel/random/boot_id"
        boot.parent.mkdir(parents=True, exist_ok=True)
        if not boot.exists():
            boot.write_text(BOOT_ID + "\n")
        heartbeat = self.home / "heartbeat"
        heartbeat.write_text("heartbeat\n")
        os.utime(heartbeat, (now, now))
        process = self.proc / str(pid)
        process.mkdir(exist_ok=True)
        fields = ["0"] * 50
        fields[0], fields[19] = "S", str(start)
        fields[23:26] = ["4194304", "4200000", "140737488300000"]
        (process / "stat").write_text(f"{pid} (clawq (worker)) " + " ".join(fields))
        if not (process / "exe").exists():
            (process / "exe").write_bytes(b"fixture image")
        (self.proc / "uptime").write_text("300.00 500.00\n")
        (process / "smaps_rollup").write_text(
            f"Rss: {rss} kB\nPss: 18000 kB\nSwap: 4000 kB\n"
        )

    def sample(self, now=1000):
        return watch.sample(self.home, self.state, self.proc, now, ticks=100)

    def healthy_row(self, ts):
        row = dict.fromkeys(watch.FIELDS, "")
        values = dict(
            ts=ts, pid=42, start_ticks=10000, status="healthy", rss_kb=24000,
            pss_kb=18000, swap_kb=4000, rss_swap_kb=28000, uptime_s=200,
            heartbeat_age_s=0, boot_id=BOOT_ID, image_id="1:2:3:4",
            runtime_id="a" * 64,
        )
        row.update({key: value for key, value in values.items() if key in row})
        return row

    def write_rows(self, rows, header=None):
        self.state.mkdir(exist_ok=True)
        with (self.state / "samples.csv").open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=watch.FIELDS if header is None else header)
            writer.writeheader()
            writer.writerows(rows)

    def test_process_uptime_and_identity(self):
        self.daemon()
        row = self.sample()
        self.assertEqual("healthy", row["status"])
        self.assertEqual(200, row["uptime_s"])
        self.assertEqual(10000, row["start_ticks"])
        self.assertEqual(28000, row["rss_swap_kb"])

    def test_dead_daemon_is_recorded_instead_of_leaving_a_gap(self):
        row = self.sample()
        self.assertEqual("dead", row["status"])
        with (self.state / "samples.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        self.assertEqual("dead", rows[0]["status"])

    def test_stale_heartbeat_cannot_count_as_clean(self):
        self.daemon()
        row = self.sample(now=1300)
        self.assertEqual("stale", row["status"])
        self.assertEqual(0, watch.report(self.state)["observed_healthy_seconds"])

    def test_gap_restarts_observation_window(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060)
        self.sample(1060)
        self.daemon(now=2000)
        self.sample(2000)
        result = watch.report(self.state, now=2000)
        self.assertEqual(0, result["observed_healthy_seconds"])
        self.assertFalse(result["fourteen_days_observed"])
        self.assertEqual(1, result["observation_gaps"])

    def test_memory_growth_is_a_health_event(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060, rss=140000)
        row = self.sample(1060)
        self.assertEqual("memory_growth", row["status"])
        self.assertEqual(0, watch.report(self.state)["observed_healthy_seconds"])

    def test_restart_does_not_compare_rss_between_process_generations(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060, rss=140000, start=20000)
        row = self.sample(1060)
        self.assertEqual("healthy", row["status"])
        result = watch.report(self.state, now=1060)
        self.assertEqual(0, result["observed_healthy_seconds"])
        self.assertEqual(1, result["runtime_boundaries"])

    def test_death_and_replacement_between_samples_breaks_observation(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060, pid=43, start=20000)
        self.sample(1060)
        result = watch.report(self.state, now=1060)
        self.assertEqual(0, result["observed_healthy_seconds"])
        self.assertEqual(1, result["runtime_boundaries"])
        self.assertEqual(1060, result["last_runtime_boundary_ts"])

    def test_same_pid_image_replacement_breaks_credit_and_growth_comparison(self):
        self.daemon()
        before = self.sample()
        replacement = self.root / "new-image"
        replacement.write_bytes(b"different runtime image")
        replacement.replace(self.proc / "42/exe")
        self.daemon(now=1060, rss=140000)
        after = self.sample(1060)
        self.assertEqual("healthy", after["status"])
        self.assertNotEqual(before["image_id"], after["image_id"])
        result = watch.report(self.state, now=1060)
        self.assertEqual(0, result["observed_healthy_seconds"])
        self.assertEqual(1, result["runtime_boundaries"])

    def test_service_metadata_replacement_breaks_credit_without_new_image(self):
        self.daemon()
        before = self.sample()
        replacement = self.home / "daemon.pid.meta.new"
        replacement.write_text("10000")
        replacement.replace(self.home / "daemon.pid.meta")
        self.daemon(now=1060, rss=140000)
        after = self.sample(1060)
        self.assertEqual("healthy", after["status"])
        self.assertEqual(before["image_id"], after["image_id"])
        self.assertNotEqual(before["runtime_id"], after["runtime_id"])
        self.assertEqual(0, watch.report(self.state, now=1060)["observed_healthy_seconds"])

    def test_changed_exec_layout_and_boot_identity_break_credit(self):
        for change in ("layout", "boot"):
            with self.subTest(change=change):
                self.daemon()
                before = self.sample()
                if change == "layout":
                    # The production NOFORK exec path preserves pid metadata.
                    self.daemon(now=1060, rss=140000)
                    stat = self.proc / "42/stat"
                    stat.write_text(stat.read_text().replace("140737488300000", "140737488200000"))
                else:
                    (self.proc / "sys/kernel/random/boot_id").write_text(
                        "17a010e1-411f-4d20-9c9e-123456789abc\n"
                    )
                heartbeat = self.home / "heartbeat"
                os.utime(heartbeat, (1060, 1060))
                after = self.sample(1060)
                self.assertEqual("healthy", after["status"])
                key = "runtime_id" if change == "layout" else "boot_id"
                self.assertNotEqual(before[key], after[key])
                self.assertEqual(0, watch.report(self.state, now=1060)["observed_healthy_seconds"])
                (self.state / "samples.csv").unlink()

    def test_missing_or_recycled_pid_metadata_is_unknown(self):
        for value in (None, "", "20000", "{\"start_ticks\":10000}", "010000", "-1"):
            with self.subTest(meta=value):
                self.daemon()
                meta = self.home / "daemon.pid.meta"
                if value is None:
                    meta.unlink()
                else:
                    meta.write_text(value)
                self.assertEqual("identity_unknown", self.sample()["status"])

    def test_missing_runtime_identity_cannot_be_healthy(self):
        for path in ("42/exe", "sys/kernel/random/boot_id"):
            with self.subTest(path=path):
                self.daemon()
                (self.proc / path).unlink()
                self.assertNotEqual("healthy", self.sample()["status"])

    def test_identity_is_revalidated_after_metric_capture(self):
        for change in ("pidfile", "metadata", "start_ticks", "image"):
            with self.subTest(change=change):
                self.daemon()
                original = Path.read_text

                def read_text(path, *args, **kwargs):
                    result = original(path, *args, **kwargs)
                    if path == self.proc / "42/smaps_rollup":
                        if change == "pidfile":
                            (self.home / "daemon.pid").write_text("43\n")
                        elif change == "metadata":
                            (self.home / "daemon.pid.meta").write_text("20000")
                        elif change == "start_ticks":
                            stat = self.proc / "42/stat"
                            stat.write_text(original(stat).replace("10000", "20000"))
                        else:
                            image = self.root / "replacement"
                            image.write_bytes(b"replacement")
                            image.replace(self.proc / "42/exe")
                    return result

                with mock.patch.object(Path, "read_text", read_text):
                    self.assertEqual("identity_changed", self.sample()["status"])

    def test_summary_does_not_claim_fourteen_days_without_samples(self):
        self.assertFalse(watch.report(self.state)["fourteen_days_observed"])
        self.daemon()
        self.sample()
        result = watch.report(self.state)
        self.assertFalse(result["fourteen_days_observed"])
        self.assertEqual(1, result["sample_count"])

    def test_existing_samples_are_preserved(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060)
        self.sample(1060)
        result = watch.report(self.state, now=1060)
        self.assertEqual(2, result["sample_count"])
        self.assertEqual(60, result["observed_healthy_seconds"])

    def test_stopped_sampler_cannot_report_a_current_clean_window(self):
        self.daemon()
        self.sample()
        self.daemon(now=1060)
        self.sample(1060)
        result = watch.report(self.state, now=2000)
        self.assertFalse(result["sampling_current"])
        self.assertEqual(0, result["observed_healthy_seconds"])

    def test_large_csv_keeps_growth_detection_with_a_bounded_tail(self):
        self.daemon()
        for now in range(1000, 1000 + 60 * 400, 60):
            self.daemon(now=now)
            self.sample(now)
        now += 60
        self.assertGreater((self.state / "samples.csv").stat().st_size, 8192 * 2)
        self.daemon(now=now, rss=140000)
        self.assertEqual("memory_growth", self.sample(now)["status"])

    def test_fourteen_days_requires_a_complete_current_observation_series(self):
        self.state.mkdir()
        with (self.state / "samples.csv").open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=watch.FIELDS)
            writer.writeheader()
            for ts in range(1000, 1000 + watch.FOURTEEN_DAYS + 1, 60):
                writer.writerow(self.healthy_row(ts))
        end = 1000 + watch.FOURTEEN_DAYS
        result = watch.report(self.state, now=end)
        self.assertTrue(result["fourteen_days_observed"])
        self.assertEqual(watch.FOURTEEN_DAYS, result["observed_healthy_seconds"])
        self.assertFalse(watch.report(self.state, now=end + 181)["fourteen_days_observed"])

    def test_empty_healthy_records_never_earn_fourteen_days(self):
        self.write_rows(
            {"ts": ts, "status": "healthy"}
            for ts in range(1000, 1000 + watch.FOURTEEN_DAYS + 1, 60)
        )
        result = watch.report(self.state, now=1000 + watch.FOURTEEN_DAYS)
        self.assertFalse(result["fourteen_days_observed"])
        self.assertEqual(0, result["observed_healthy_seconds"])
        self.assertFalse(result["evidence_complete"])

    def test_malformed_rows_break_observation_and_collection_recovers(self):
        for field, value in (("ts", "bad"), ("rss_swap_kb", ""), ("rss_kb", "-1"),
                             ("pss_kb", "no"), ("heartbeat_age_s", "nan"),
                             ("rss_swap_kb", "2"), ("pid", "0")):
            with self.subTest(field=field, value=value):
                bad = self.healthy_row(1060)
                bad[field] = value
                self.write_rows([self.healthy_row(1000), bad])
                raw = (self.state / "samples.csv").read_bytes()
                self.daemon(now=1120, rss=140000)
                self.assertEqual("healthy", self.sample(1120)["status"])
                self.assertTrue((self.state / "samples.csv").read_bytes().startswith(raw))
                result = watch.report(self.state, now=1120)
                self.assertEqual(0, result["observed_healthy_seconds"])
                self.assertFalse(result["evidence_complete"])
                self.assertEqual(1, result["invalid_records"])

    def test_unterminated_tail_is_preserved_separated_and_invalidated(self):
        for tail in (b"1060,42,10000,healthy", b'"broken', b"\xffinvalid",
                     self.csv_record(self.healthy_row(1060)).rstrip(b"\r\n")):
            with self.subTest(tail=tail[:30]):
                self.write_rows([self.healthy_row(1000)])
                with (self.state / "samples.csv").open("ab") as stream:
                    stream.write(tail)
                raw = (self.state / "samples.csv").read_bytes()
                self.daemon(now=1120)
                self.sample(1120)
                data = (self.state / "samples.csv").read_bytes()
                self.assertTrue(data.startswith(raw + b"\n"))
                result = watch.report(self.state, now=1120)
                self.assertFalse(result["evidence_complete"])
                self.assertGreater(result["invalid_records"], 0)
                self.assertEqual(0, result["observed_healthy_seconds"])
                self.assertEqual("healthy", result["last_sample"]["status"])

    @staticmethod
    def csv_record(row):
        stream = io.StringIO(newline="")
        csv.DictWriter(stream, fieldnames=watch.FIELDS).writerow(row)
        return stream.getvalue().encode()

    def test_bad_headers_do_not_stop_collection_or_earn_credit(self):
        for header in ("ts,status", ",".join(reversed(watch.FIELDS)), "\"broken", ""):
            with self.subTest(header=header):
                self.state.mkdir(exist_ok=True)
                raw = (header + "\n1000,healthy\n").encode()
                (self.state / "samples.csv").write_bytes(raw)
                self.daemon(now=1060)
                self.sample(1060)
                self.assertTrue((self.state / "samples.csv").read_bytes().startswith(raw))
                result = watch.report(self.state, now=1060)
                self.assertFalse(result["evidence_complete"])
                self.assertFalse(result["sampling_current"])
                self.assertEqual(0, result["observed_healthy_seconds"])

    def test_malformed_historical_evidence_replaces_old_successful_summary(self):
        self.write_rows([self.healthy_row(1000), dict(self.healthy_row(1060), ts="bad")])
        (self.state / "summary.json").write_text('{"fourteen_days_observed":true}\n')
        with mock.patch.object(sys, "argv", [str(SCRIPT), "report", "--state-dir",
                                            str(self.state), "--write-summary"]), \
                contextlib.redirect_stdout(io.StringIO()):
            watch.main()
        result = json.loads((self.state / "summary.json").read_text())
        self.assertFalse(result["fourteen_days_observed"])
        self.assertFalse(result["evidence_complete"])
        self.assertEqual(1, result["invalid_records"])

    def test_overlapping_cli_summaries_publish_complete_snapshots_in_order(self):
        self.daemon()
        self.sample()
        ctx = multiprocessing.get_context("fork")
        captured, release, started = ctx.Event(), ctx.Event(), ctx.Event()
        second_captured, second_started = ctx.Event(), ctx.Event()
        results = ctx.Queue()
        self.addCleanup(results.close)
        first = ctx.Process(target=summary_writer,
                            args=(self.state, captured, release, started, results, True))
        second = ctx.Process(target=summary_writer,
                             args=(self.state, second_captured, release, second_started, results, False))
        first.start()
        try:
            self.assertTrue(captured.wait(10))
            self.daemon(now=1060)
            self.sample(1060)
            second.start()
            self.assertTrue(second_started.wait(10))
            newer_generated_before_release = second_captured.wait(0.5)
        finally:
            release.set()
            first.join(10)
            if second.pid is not None:
                second.join(10)
            for process in (first, second):
                if process.is_alive():
                    process.terminate()
                    process.join()
        self.assertEqual(0, first.exitcode)
        self.assertEqual(0, second.exitcode)
        outputs = [results.get(timeout=2), results.get(timeout=2)]
        self.assertEqual([None, None], [error for error, _ in outputs])
        result = json.loads((self.state / "summary.json").read_text())
        self.assertEqual(2, result["sample_count"])
        self.assertEqual([1, 2], sorted(value["sample_count"] for _, value in outputs))
        self.assertFalse(newer_generated_before_release)


if __name__ == "__main__":
    unittest.main()
