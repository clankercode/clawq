#!/usr/bin/env python3

import csv
import shutil
import statistics
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
DIST = ROOT / "dist" / "packaging-report"
BASE_EXE = ROOT / "_build_opt_size" / "default" / "src" / "main.exe"
TSV = DIST / "results.tsv"
MD = DIST / "report.md"


def median_time(command: list[str], runs: int = 5) -> float:
    values = []
    for _ in range(runs):
        start = time.perf_counter()
        subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        values.append(time.perf_counter() - start)
    return statistics.median(values)


def size_bytes(path: Path) -> int:
    return path.stat().st_size


def reset_copy(src: Path, dst: Path) -> None:
    if dst.exists():
        dst.chmod(dst.stat().st_mode | 0o200)
        dst.unlink()
    shutil.copy2(src, dst)
    dst.chmod(dst.stat().st_mode | 0o200)


def measure_variant(label: str, path: Path, notes: str) -> dict[str, str]:
    return {
        "variant": label,
        "path": str(path),
        "size_bytes": str(size_bytes(path)),
        "help_median_s": f"{median_time([str(path), '--help']):.6f}",
        "status_median_s": f"{median_time([str(path), 'status']):.6f}",
        "notes": notes,
    }


def pct_delta(new: str, old: str) -> str:
    n = float(new)
    o = float(old)
    if o == 0:
        return "n/a"
    return f"{((n - o) / o) * 100:+.2f}%"


def main() -> None:
    DIST.mkdir(parents=True, exist_ok=True)
    if not BASE_EXE.exists():
        raise SystemExit(f"Missing optimized binary: {BASE_EXE}")

    rows = []

    unstripped = DIST / "clawq-size-unstripped"
    reset_copy(BASE_EXE, unstripped)
    rows.append(measure_variant("unstripped", unstripped, "Baseline optimized size build"))

    stripped = DIST / "clawq-size-stripped"
    reset_copy(BASE_EXE, stripped)
    subprocess.run(["strip", str(stripped)], check=True)
    rows.append(measure_variant("stripped", stripped, "Recommended default release artifact"))

    upx = shutil.which("upx")
    if upx:
        upx_path = DIST / "clawq-size-stripped-upx"
        reset_copy(stripped, upx_path)
        subprocess.run([upx, "--best", str(upx_path)], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        rows.append(measure_variant("stripped-upx", upx_path, "Optional UPX-packed artifact"))
    else:
        rows.append(
            {
                "variant": "stripped-upx",
                "path": "n/a",
                "size_bytes": "n/a",
                "help_median_s": "n/a",
                "status_median_s": "n/a",
                "notes": "UPX not installed on this machine; skipped measurement",
            }
        )

    with TSV.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=["variant", "path", "size_bytes", "help_median_s", "status_median_s", "notes"],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(rows)

    baseline = rows[0]
    lines = [
        "# Packaging Report",
        "",
        "This report compares release packaging variants for the size-optimized binary.",
        "Startup proxy uses `--help`; runtime proxy uses `status`.",
        "",
        "| variant | size bytes | help median (s) | status median (s) | notes |",
        "|---|---:|---:|---:|---|",
    ]
    for row in rows:
        lines.append(
            f"| {row['variant']} | {row['size_bytes']} | {row['help_median_s']} | {row['status_median_s']} | {row['notes']} |"
        )
    lines.extend(["", "## Delta vs unstripped", "", "| variant | size delta | help delta | status delta |", "|---|---:|---:|---:|"])
    for row in rows[1:]:
        if row["size_bytes"] == "n/a":
            lines.append(f"| {row['variant']} | n/a | n/a | n/a |")
        else:
            lines.append(
                f"| {row['variant']} | {pct_delta(row['size_bytes'], baseline['size_bytes'])} | {pct_delta(row['help_median_s'], baseline['help_median_s'])} | {pct_delta(row['status_median_s'], baseline['status_median_s'])} |"
            )

    guidance = [
        "Use the stripped size build as the default release artifact.",
    ]
    if upx:
        stripped_row = next(row for row in rows if row["variant"] == "stripped")
        upx_row = next(row for row in rows if row["variant"] == "stripped-upx")
        guidance.append(
            "Only ship UPX if the extra size win matters enough to justify any measured startup/runtime regression and added operational risk."
        )
        if float(upx_row["help_median_s"]) > float(stripped_row["help_median_s"]):
            guidance.append("Current measurement shows UPX is slower than plain stripped on startup proxy, so keep it optional rather than default.")
        else:
            guidance.append("Current measurement does not show a startup regression large enough to reject UPX automatically, but it should still stay optional.")
    else:
        guidance.append("UPX was unavailable here, so release guidance should stay at strip-by-default and treat UPX as an opt-in follow-up experiment.")

    lines.extend(["", "## Guidance", ""])
    for item in guidance:
        lines.append(f"- {item}")

    MD.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {TSV}")
    print(f"Wrote {MD}")


if __name__ == "__main__":
    main()
