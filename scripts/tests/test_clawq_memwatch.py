"""Memory-watch evidence must distinguish observation from elapsed time."""

import csv
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "clawq_memwatch.py"
spec = importlib.util.spec_from_file_location("clawq_memwatch", SCRIPT)
watch = importlib.util.module_from_spec(spec)
spec.loader.exec_module(watch)


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

    def daemon(self, now=1000, rss=24000, start=10000):
        (self.home / "daemon.pid").write_text("42\n")
        heartbeat = self.home / "heartbeat"
        heartbeat.write_text("heartbeat\n")
        os.utime(heartbeat, (now, now))
        process = self.proc / "42"
        process.mkdir(exist_ok=True)
        fields = ["0"] * 50
        fields[0], fields[19] = "S", str(start)
        (process / "stat").write_text("42 (clawq (worker)) " + " ".join(fields))
        (self.proc / "uptime").write_text("300.00 500.00\n")
        (process / "smaps_rollup").write_text(
            f"Rss: {rss} kB\nPss: 18000 kB\nSwap: 4000 kB\n"
        )

    def sample(self, now=1000):
        return watch.sample(self.home, self.state, self.proc, now, ticks=100)

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
        for now in range(1000, 1000 + 60 * 100, 60):
            self.daemon(now=now)
            self.sample(now)
        now += 60
        self.daemon(now=now, rss=140000)
        self.assertEqual("memory_growth", self.sample(now)["status"])

    def test_fourteen_days_requires_a_complete_current_observation_series(self):
        self.state.mkdir()
        with (self.state / "samples.csv").open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=watch.FIELDS)
            writer.writeheader()
            for ts in range(1000, 1000 + watch.FOURTEEN_DAYS + 1, 60):
                writer.writerow({"ts": ts, "status": "healthy"})
        end = 1000 + watch.FOURTEEN_DAYS
        result = watch.report(self.state, now=end)
        self.assertTrue(result["fourteen_days_observed"])
        self.assertEqual(watch.FOURTEEN_DAYS, result["observed_healthy_seconds"])
        self.assertFalse(watch.report(self.state, now=end + 181)["fourteen_days_observed"])


if __name__ == "__main__":
    unittest.main()
