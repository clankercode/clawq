#!/usr/bin/env python3

import argparse
import csv
import re
import subprocess
from pathlib import Path


def parse_args() -> argparse.Namespace:
    root = Path(__file__).resolve().parent.parent
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--build-dir",
        default=str(root / "_build_opt_size" / "default" / "src"),
    )
    parser.add_argument(
        "--output-dir",
        default=str(root / "dist" / "native-size-report"),
    )
    parser.add_argument("--top-modules", type=int, default=20)
    parser.add_argument("--top-symbols", type=int, default=30)
    return parser.parse_args()


def run_text(command: list[str]) -> str:
    return subprocess.check_output(command, text=True)


def section_sizes(object_path: Path) -> dict[str, int]:
    output = run_text(["size", "-A", "-d", str(object_path)])
    sizes: dict[str, int] = {}
    for line in output.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0].startswith("."):
            try:
                sizes[parts[0]] = int(parts[1])
            except ValueError:
                pass
    return sizes


def symbol_rows(object_path: Path, group: str, module: str) -> list[dict[str, str]]:
    output = run_text(["nm", "-S", "--size-sort", "--print-size", str(object_path)])
    rows = []
    for line in output.splitlines():
        match = re.match(r"^[0-9a-fA-F]+\s+([0-9a-fA-F]+)\s+([A-Za-z])\s+(.+)$", line.strip())
        if not match:
            continue
        size_hex, symbol_type, name = match.groups()
        if symbol_type.upper() != "T":
            continue
        size = int(size_hex, 16)
        if size <= 0:
            continue
        if name.endswith(".code_begin") or name.endswith(".code_end") or name.endswith(".entry"):
            continue
        rows.append(
            {
                "group": group,
                "module": module,
                "symbol_type": symbol_type,
                "size": str(size),
                "symbol": name,
            }
        )
    return rows


def collect(build_dir: Path) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    object_sets = [
        ("core", build_dir / ".clawq_runtime_core.objs" / "native"),
        ("integrations", build_dir / ".clawq_runtime_integrations.objs" / "native"),
        ("main", build_dir / ".main.eobjs" / "native"),
    ]
    modules = []
    symbols = []
    for group, directory in object_sets:
        for object_path in sorted(directory.glob("*.o")):
            module = object_path.stem
            sizes = section_sizes(object_path)
            text = sizes.get(".text", 0)
            rodata = sizes.get(".rodata", 0)
            data = sizes.get(".data", 0)
            bss = sizes.get(".bss", 0)
            debug = sum(value for key, value in sizes.items() if key.startswith(".debug"))
            eh_frame = sizes.get(".eh_frame", 0)
            footprint = text + rodata + data + bss + eh_frame
            modules.append(
                {
                    "group": group,
                    "module": module,
                    "object_path": str(object_path),
                    "text": str(text),
                    "rodata": str(rodata),
                    "data": str(data),
                    "bss": str(bss),
                    "eh_frame": str(eh_frame),
                    "debug": str(debug),
                    "footprint": str(footprint),
                }
            )
            symbols.extend(symbol_rows(object_path, group, module))
    modules.sort(key=lambda row: (-int(row["footprint"]), -int(row["text"]), row["module"]))
    symbols.sort(key=lambda row: (-int(row["size"]), row["symbol"]))
    return modules, symbols


def module_takeaway(module: str, group: str) -> str:
    if module in {"ws_client", "http_server", "ui_server", "provider_anthropic", "provider_gemini"}:
        return "integration hotspot; good candidate for targeted size work or optional packaging"
    if module in {"memory", "session", "provider", "tools_builtin"}:
        return "core hotspot; optimize only after integration-heavy modules"
    if group == "integrations":
        return "integration-side contributor; check if feature can stay outside minimal/core builds"
    return "shared/core contributor; optimize only if feature is performance-critical or always-on"


def write_outputs(output_dir: Path, modules: list[dict[str, str]], symbols: list[dict[str, str]], top_modules: int, top_symbols: int) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    modules_tsv = output_dir / "modules.tsv"
    symbols_tsv = output_dir / "symbols.tsv"
    report_md = output_dir / "report.md"

    with modules_tsv.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=["group", "module", "object_path", "text", "rodata", "data", "bss", "eh_frame", "debug", "footprint"],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(modules)

    with symbols_tsv.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=["group", "module", "symbol_type", "size", "symbol"],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(symbols)

    top_module_rows = modules[:top_modules]
    top_symbol_rows = symbols[:top_symbols]
    lines = [
        "# Native Size Report",
        "",
        "This report inspects native object files from `_build_opt_size/default/src` after `make build-opt-size`.",
        "`footprint` is `.text + .rodata + .data + .bss + .eh_frame` for each object file.",
        "The symbol table comes from `nm -S --size-sort --print-size` on those object files.",
        "",
        "## Top Modules",
        "",
        "| group | module | footprint | text | rodata | data | eh_frame | takeaway |",
        "|---|---|---:|---:|---:|---:|---:|---|",
    ]
    for row in top_module_rows:
        lines.append(
            f"| {row['group']} | {row['module']} | {row['footprint']} | {row['text']} | {row['rodata']} | {row['data']} | {row['eh_frame']} | {module_takeaway(row['module'], row['group'])} |"
        )
    lines.extend(
        [
            "",
            "## Top Symbols",
            "",
            "| group | module | type | size | symbol |",
            "|---|---|---|---:|---|",
        ]
    )
    for row in top_symbol_rows:
        lines.append(
            f"| {row['group']} | {row['module']} | {row['symbol_type']} | {row['size']} | {row['symbol']} |"
        )
    lines.extend(
        [
            "",
            "Full data is available in `modules.tsv` and `symbols.tsv`.",
        ]
    )
    report_md.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {modules_tsv}")
    print(f"Wrote {symbols_tsv}")
    print(f"Wrote {report_md}")


def main() -> None:
    args = parse_args()
    modules, symbols = collect(Path(args.build_dir))
    write_outputs(Path(args.output_dir), modules, symbols, args.top_modules, args.top_symbols)


if __name__ == "__main__":
    main()
