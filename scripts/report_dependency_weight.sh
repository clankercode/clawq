#!/usr/bin/env python3

import argparse
import csv
import os
import re
import subprocess
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--switch", default=os.environ.get("SHELL_SWITCH", "clawq-5.1"))
    parser.add_argument(
        "--opam-file",
        default=str(Path(__file__).resolve().parent.parent / "clawq.opam"),
    )
    parser.add_argument(
        "--output-dir",
        default=str(Path(__file__).resolve().parent.parent / "dist" / "dependency-audit"),
    )
    return parser.parse_args()


def read_direct_deps(opam_file: Path) -> list[str]:
    deps = []
    in_dep_block = False
    for raw_line in opam_file.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if line == "depends: [":
            in_dep_block = True
            continue
        if in_dep_block and line == "]":
            break
        if not in_dep_block:
            continue
        match = re.match(r'"([^"]+)"', line)
        if match:
            deps.append(match.group(1))
    return deps


def run_text(command: list[str]) -> str:
    return subprocess.check_output(command, text=True, stderr=subprocess.DEVNULL)


def tree_packages(opam_switch: str, package: str) -> list[str]:
    output = run_text(["opam", "tree", f"--switch={opam_switch}", "--no-constraint", package])
    packages = []
    for line in output.splitlines():
        stripped = re.sub(r"^[\s\|├└─]+", "", line).strip()
        match = re.match(r"([A-Za-z0-9_.+-]+?)\.(?:v?\d|base)", stripped)
        if match:
            packages.append(match.group(1))
    return sorted(set(packages))


def lib_size_kb(opam_switch: str, package: str) -> int:
    try:
        lib_path = run_text(["opam", "var", f"--switch={opam_switch}", f"{package}:lib"]).strip()
    except subprocess.CalledProcessError:
        return 0
    if not lib_path:
        return 0
    lib_dir = Path(lib_path)
    if not lib_dir.exists():
        return 0
    size_output = run_text(["du", "-sk", str(lib_dir)])
    return int(size_output.split()[0])


def recommendation(package: str) -> str:
    if package in {"coq", "coq-stdlib"}:
        return "build-only toolchain; keep out of runtime packaging paths"
    if package in {"cohttp-lwt-unix", "conduit-lwt-unix", "tls-lwt", "ca-certs"}:
        return "integration-heavy network stack; isolate behind integration-only packages"
    if package == "sqlite3":
        return "persistence dependency; consider optional storage backend only if minimal mode can stay file-free"
    if package in {"mirage-crypto", "mirage-crypto-rng", "kdf", "digestif", "base64"}:
        return "shared crypto stack; avoid expanding it further into minimal-only code paths"
    if package in {"cmdliner", "yojson", "lwt", "logs", "fmt"}:
        return "foundational dependency; replacement likely low ROI"
    return "shared support dependency; replacement probably lower priority than isolating integration stacks"


def write_outputs(output_dir: Path, rows: list[dict[str, str]]) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    tsv_path = output_dir / "direct-dependency-weight.tsv"
    md_path = output_dir / "report.md"

    with tsv_path.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "package",
                "closure_packages",
                "closure_lib_kb",
                "recommendation",
            ],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(rows)

    top_rows = rows[:10]
    lines = [
        "# Dependency Weight Audit",
        "",
        "This report is a heuristic audit of direct dependencies from `clawq.opam`.",
        "`closure_packages` counts unique packages visible in `opam tree` for each direct dependency.",
        "`closure_lib_kb` sums installed library directories for those packages, so shared packages are intentionally over-counted as a rough weight signal.",
        "",
        "| package | closure packages | closure lib KB | recommendation |",
        "|---|---:|---:|---|",
    ]
    for row in top_rows:
        lines.append(
            f"| {row['package']} | {row['closure_packages']} | {row['closure_lib_kb']} | {row['recommendation']} |"
        )
    lines.extend(
        [
            "",
            "Full data is available in `direct-dependency-weight.tsv`.",
        ]
    )
    md_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {tsv_path}")
    print(f"Wrote {md_path}")


def main() -> None:
    args = parse_args()
    opam_file = Path(args.opam_file)
    deps = read_direct_deps(opam_file)
    rows = []
    for dep in deps:
        closure = tree_packages(args.switch, dep)
        closure_lib_kb = sum(lib_size_kb(args.switch, pkg) for pkg in closure)
        rows.append(
            {
                "package": dep,
                "closure_packages": str(len(closure)),
                "closure_lib_kb": str(closure_lib_kb),
                "recommendation": recommendation(dep),
            }
        )
    rows.sort(key=lambda row: (-int(row["closure_packages"]), -int(row["closure_lib_kb"]), row["package"]))
    write_outputs(Path(args.output_dir), rows)


if __name__ == "__main__":
    main()
