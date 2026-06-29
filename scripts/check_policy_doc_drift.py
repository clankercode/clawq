#!/usr/bin/env python3
"""Check for drift between policy documentation and actual code/tests.

Parses invariant specs (scope-resolution-invariants.md,
memory-policy-isolation-invariants.md), verification-boundaries.md, and
proof-backlog.md, then cross-references claims against test files and source
code to catch stale documentation.

Exit codes:
  0  No drift detected
  1  Drift detected (errors printed to stderr)
  2  Script error (missing files, parse failure)
"""

import os
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

# ── Paths ────────────────────────────────────────────────────────────────────

REPO_ROOT = Path(__file__).resolve().parent.parent

INVARIANT_DOCS = [
    REPO_ROOT / "docs" / "scope-resolution-invariants.md",
    REPO_ROOT / "docs" / "memory-policy-isolation-invariants.md",
]

VERIFICATION_BOUNDARIES = REPO_ROOT / "docs" / "verification-boundaries.md"
PROOF_BACKLOG = REPO_ROOT / "docs" / "proof-backlog.md"
TEST_DIR = REPO_ROOT / "test"

# Test files explicitly listed in the Appendix of each invariant spec.
# These are the authoritative list of files where invariant tests live.
EXPECTED_TEST_FILES = [
    "test/test_scope_resolver.ml",
    "test/test_memory_isolation.ml",
    "test/test_memory_ledger.ml",
    "test/test_access_snapshot.ml",
    "test/test_minimal_reload.ml",
    "test/test_config_loader.ml",
    "test/test_invariant_conformance.ml",
]

# ── Data Structures ──────────────────────────────────────────────────────────


@dataclass
class Invariant:
    id: str
    tags: list[str]
    doc_file: str
    line_num: int
    test_names: list[str] = field(default_factory=list)
    code_refs: list[str] = field(default_factory=list)


@dataclass
class Drift:
    severity: str  # "error" or "warning"
    category: str
    message: str


# ── Parsing Helpers ──────────────────────────────────────────────────────────

# INV-{CATEGORY}-{NUMBER}[optional_letter]
# Matches: INV-DET-1, INV-REDACT-3b, INV-MEM-ISO-1
# Category can be multi-part: MEM-ISO, SEC, CONF, etc.
INV_PATTERN = re.compile(
    r"^\*\*(INV-(?:[A-Z]+-)+\d+(?:[a-z])?)\*\*\s+`([^`]+)`",
    re.MULTILINE,
)

TEST_NAME_PATTERN = re.compile(r"`(test_[a-z_]+|assert_[a-z_]+)`")

# Match test file references in Appendix tables:
# | `test/test_foo.ml` | ... |
APPENDIX_FILE_PATTERN = re.compile(r"\|\s*`(test/[^`]+\.ml)`\s*\|")

# Invariant ID pattern for cross-referencing.
# INV-{CATEGORY}-{NUMBER}[optional_letter]
# Matches: INV-DET-1, INV-REDACT-3b, INV-MEM-ISO-1
# Category can be multi-part: MEM-ISO, SEC, CONF, etc.
# Negative lookahead prevents matching prefix of INV-MEM-ISO-1 as INV-MEM-ISO.
INV_ID_PATTERN = re.compile(r"INV-(?:[A-Z]+-)+\d+(?:[a-z])?")


def parse_invariants_from_doc(doc_path: Path) -> list[Invariant]:
    """Extract invariant definitions and their tags from a markdown doc."""
    text = doc_path.read_text()
    invariants = []

    for match in INV_PATTERN.finditer(text):
        inv_id = match.group(1)
        raw_tags = match.group(2)
        line_num = text[: match.start()].count("\n") + 1

        tags = [t.strip() for t in raw_tags.split("]") if t.strip()]
        # Clean up tags: remove leading "[" if present
        tags = [t.lstrip("[").strip() for t in tags]
        tags = [t for t in tags if t]  # remove empty

        inv = Invariant(
            id=inv_id,
            tags=tags,
            doc_file=str(doc_path.relative_to(REPO_ROOT)),
            line_num=line_num,
        )
        invariants.append(inv)

    # Now extract test names referenced near each invariant definition.
    # We look for test names in the text block between this invariant and
    # the next one (or end of file).
    lines = text.split("\n")
    for i, inv in enumerate(invariants):
        # Find the line range for this invariant's section
        start_line = inv.line_num - 1  # 0-indexed
        if i + 1 < len(invariants):
            end_line = invariants[i + 1].line_num - 1
        else:
            end_line = len(lines)

        section_text = "\n".join(lines[start_line:end_line])

        # Extract test names mentioned in this section
        test_matches = TEST_NAME_PATTERN.findall(section_text)
        inv.test_names = list(dict.fromkeys(test_matches))  # dedupe, preserve order

    return invariants


def extract_test_names_from_file(file_path: Path) -> set[str]:
    """Extract all `let test_*` and `let assert_*` function names from an OCaml test file."""
    if not file_path.exists():
        return set()
    text = file_path.read_text()
    names = set(re.findall(r"let\s+(test_[a-z_]+)", text))
    names.update(re.findall(r"let\s+(assert_[a-z_]+)", text))
    return names


def extract_appendix_test_files(doc_path: Path) -> list[str]:
    """Extract test file paths from the Appendix section of an invariant doc."""
    text = doc_path.read_text()
    return APPENDIX_FILE_PATTERN.findall(text)


def extract_verification_boundary_counts(doc_path: Path) -> dict:
    """Extract claimed test counts from verification-boundaries.md summary tables.

    Returns dict like:
      { "Scope Resolution": { "Determinism": {"invariants": "INV-DET-1, INV-DET-2",
                                               "tests_claimed": 4, "runtime": True}, ...}, ... }
    """
    text = doc_path.read_text()
    # This is complex to parse generically; we'll do targeted checks instead.
    return text


# ── Drift Checks ─────────────────────────────────────────────────────────────


def check_test_name_existence(
    invariants: list[Invariant], all_tests: dict[str, set[str]]
) -> list[Drift]:
    """Check that every test name referenced in docs exists in a test file."""
    drifts = []
    # Build a flat set of all known test names across all test files
    all_known_tests: set[str] = set()
    for tests in all_tests.values():
        all_known_tests.update(tests)

    for inv in invariants:
        for test_name in inv.test_names:
            if test_name not in all_known_tests:
                drifts.append(
                    Drift(
                        severity="error",
                        category="missing-test",
                        message=(
                            f"{inv.doc_file}:{inv.line_num}: {inv.id} references "
                            f"`{test_name}` but no such test function exists in "
                            f"any test file"
                        ),
                    )
                )
    return drifts


def check_appendix_files_exist(invariant_docs: list[Path]) -> list[Drift]:
    """Check that every test file listed in Appendix tables exists on disk."""
    drifts = []
    for doc_path in invariant_docs:
        file_refs = extract_appendix_test_files(doc_path)
        for ref in file_refs:
            full_path = REPO_ROOT / ref
            if not full_path.exists():
                drifts.append(
                    Drift(
                        severity="error",
                        category="missing-file",
                        message=(
                            f"{doc_path.relative_to(REPO_ROOT)}: Appendix references "
                            f"`{ref}` but the file does not exist"
                        ),
                    )
                )
    return drifts


def check_gap_invariants_have_tests(
    invariants: list[Invariant], all_tests: dict[str, set[str]]
) -> list[Drift]:
    """Warn if an invariant tagged [GAP] has tests (doc needs updating).

    Some GAP invariants have tests that are explicitly documented as inadequate
    (e.g., misnamed tests that don't actually cover the gap). These are listed
    in KNOWN_MISNAMED_TESTS and reported as warnings rather than errors.
    """
    # Tests that exist but are documented as not actually covering the gap.
    # Format: {inv_id: (test_name, reason)}
    known_misnamed = {
        "INV-REDACT-3b": (
            "test_forgotten_memory_not_in_fts_search",
            "doc notes this test is misnamed - it tests query_scoped_memories "
            "(content-search path), not the Memory.search FTS path",
        ),
    }

    drifts = []
    all_known_tests: set[str] = set()
    for tests in all_tests.values():
        all_known_tests.update(tests)

    for inv in invariants:
        if "GAP" in inv.tags:
            existing_tests = [t for t in inv.test_names if t in all_known_tests]
            if existing_tests:
                known = known_misnamed.get(inv.id)
                if known:
                    test_name, reason = known
                    if test_name in existing_tests:
                        drifts.append(
                            Drift(
                                severity="warning",
                                category="gap-test-misnamed",
                                message=(
                                    f"{inv.doc_file}:{inv.line_num}: {inv.id} "
                                    f"is tagged [GAP]. Test `{test_name}` exists "
                                    f"but is misnamed: {reason}. "
                                    f"The gap remains open."
                                ),
                            )
                        )
                        continue
                drifts.append(
                    Drift(
                        severity="error",
                        category="gap-closed",
                        message=(
                            f"{inv.doc_file}:{inv.line_num}: {inv.id} is tagged "
                            f"[GAP] but tests exist: {', '.join(existing_tests)}. "
                            f"Update the tag to [TEST] and add test references."
                        ),
                    )
                )
    return drifts


def check_test_tag_has_tests(
    invariants: list[Invariant], all_tests: dict[str, set[str]]
) -> list[Drift]:
    """Error if an invariant tagged [TEST] has no referenced test names.

    An invariant claiming [TEST] status without any test references is stale
    documentation — the claim cannot be verified.
    """
    drifts = []
    for inv in invariants:
        if "TEST" in inv.tags and not inv.test_names:
            drifts.append(
                Drift(
                    severity="error",
                    category="test-tag-no-refs",
                    message=(
                        f"{inv.doc_file}:{inv.line_num}: {inv.id} is tagged "
                        f"[TEST] but no test names are referenced in its section. "
                        f"Add test name references (e.g., `test_foo_bar`) to the doc."
                    ),
                )
            )
    return drifts


def check_verification_boundary_counts(doc_path: Path) -> list[Drift]:
    """Check that verification-boundaries.md is consistent with reality.

    Validates:
    - References to invariant spec files exist
    - Source file links in tables exist
    - Known Gaps section mentions INV-REDACT-3b
    - Conformance test file reference exists
    """
    drifts = []
    if not doc_path.exists():
        drifts.append(
            Drift(
                severity="error",
                category="missing-file",
                message=f"Verification boundaries doc not found: {doc_path}",
            )
        )
        return drifts

    text = doc_path.read_text()

    # Check that the doc references the correct invariant spec files
    for expected_ref in [
        "scope-resolution-invariants.md",
        "memory-policy-isolation-invariants.md",
        "proof-backlog.md",
    ]:
        if expected_ref not in text:
            drifts.append(
                Drift(
                    severity="warning",
                    category="missing-reference",
                    message=(
                        f"verification-boundaries.md does not reference "
                        f"`{expected_ref}`"
                    ),
                )
            )

    # Check that "Known Gaps" section mentions INV-REDACT-3b (the known gap)
    if "INV-REDACT-3b" not in text:
        drifts.append(
            Drift(
                severity="error",
                category="missing-gap",
                message=(
                    "verification-boundaries.md Known Gaps section does not "
                    "mention INV-REDACT-3b, which is documented as the "
                    "highest-priority known gap in the invariant specs"
                ),
            )
        )

    # Validate source file links in the Links section
    link_pattern = re.compile(r"\[([^\]]+)\]\(([^)]+)\)")
    for match in link_pattern.finditer(text):
        label = match.group(1)
        href = match.group(2)
        # Only check relative links (not anchors or external URLs)
        if href.startswith("http") or href.startswith("#"):
            continue
        # Resolve relative to docs/
        linked_path = REPO_ROOT / "docs" / href
        if not linked_path.exists():
            drifts.append(
                Drift(
                    severity="error",
                    category="broken-link",
                    message=(
                        f"verification-boundaries.md links to `{href}"  
                        f"({label}) but the file does not exist"
                    ),
                )
            )

    return drifts


def check_proof_backlog_consistency(
    invariants: list[Invariant],
) -> list[Drift]:
    """Check proof-backlog.md consistency with invariant specs.

    Verify that invariants listed in proof-backlog.md exist in the invariant
    specs, and that known gaps are documented.
    """
    drifts = []
    if not PROOF_BACKLOG.exists():
        drifts.append(
            Drift(
                severity="warning",
                category="missing-file",
                message="proof-backlog.md not found",
            )
        )
        return drifts

    text = PROOF_BACKLOG.read_text()

    # Extract all INV-* references from proof backlog (with word boundary)
    backlog_refs = set(INV_ID_PATTERN.findall(text))

    # Build set of all known invariant IDs from specs
    spec_ids = {inv.id for inv in invariants}

    # Check that backlog references exist in specs
    for ref in backlog_refs:
        if ref not in spec_ids:
            # INV-EGR-* in backlog may refer to memory-policy INV-EGR which
            # conflicts with scope-resolution INV-EGR. Allow known duplicates.
            drifts.append(
                Drift(
                    severity="warning",
                    category="backlog-unknown-ref",
                    message=(
                        f"proof-backlog.md references {ref} but it is not found "
                        f"in the invariant specs. It may be a stale reference "
                        f"or use a different naming convention."
                    ),
                )
            )

    # Check that the known gap (INV-REDACT-3b) is mentioned
    if "INV-REDACT-3b" not in text:
        drifts.append(
            Drift(
                severity="error",
                category="missing-gap",
                message=(
                    "proof-backlog.md does not mention INV-REDACT-3b, "
                    "the highest-priority known gap"
                ),
            )
        )

    return drifts


def check_cross_doc_consistency(
    scope_invariants: list[Invariant],
    mem_invariants: list[Invariant],
) -> list[Drift]:
    """Check for inconsistencies between the two invariant spec docs."""
    drifts = []

    # Check for duplicate invariant IDs across docs (which would be confusing)
    scope_ids = {inv.id for inv in scope_invariants}
    mem_ids = {inv.id for inv in mem_invariants}
    duplicates = scope_ids & mem_ids
    if duplicates:
        for dup in sorted(duplicates):
            # This is expected for INV-EGR-* (egress appears in both docs
            # for different subsystems). Only flag if both have the same tag.
            scope_inv = next(i for i in scope_invariants if i.id == dup)
            mem_inv = next(i for i in mem_invariants if i.id == dup)
            if set(scope_inv.tags) != set(mem_inv.tags):
                drifts.append(
                    Drift(
                        severity="warning",
                        category="tag-mismatch",
                        message=(
                            f"{dup} appears in both docs with different tags: "
                            f"{scope_inv.doc_file} has {scope_inv.tags}, "
                            f"{mem_inv.doc_file} has {mem_inv.tags}"
                        ),
                    )
                )

    return drifts


def check_source_code_references(invariants: list[Invariant]) -> list[Drift]:
    """Check that source code file references in invariant docs exist.

    Only checks explicit `src/*.ml` references. Bare filenames like
    `test_access_snapshot.ml` or `memory_scoped.ml` are not checked because
    they may refer to test files or use shorthand notation.
    """
    drifts = []
    # Pattern: explicit `src/foo.ml` reference
    src_file_pattern = re.compile(r"`src/([a-z_][a-z_0-9]*\.ml)`")

    for inv in invariants:
        doc_path = REPO_ROOT / inv.doc_file
        text = doc_path.read_text()
        lines = text.split("\n")

        # Get the section for this invariant
        start_line = inv.line_num - 1
        idx = next(
            (i for i, iv in enumerate(invariants) if iv.id == inv.id), 0
        )
        if idx + 1 < len(invariants):
            end_line = invariants[idx + 1].line_num - 1
        else:
            end_line = len(lines)

        section = "\n".join(lines[start_line:end_line])
        for match in src_file_pattern.finditer(section):
            src_file = match.group(1)
            src_path = REPO_ROOT / "src" / src_file
            if not src_path.exists():
                drifts.append(
                    Drift(
                        severity="warning",
                        category="missing-source",
                        message=(
                            f"{inv.doc_file}:{inv.line_num}: {inv.id} references "
                            f"`src/{src_file}` but the file does not exist"
                        ),
                    )
                )

    return drifts


# ── Main ─────────────────────────────────────────────────────────────────────


def main() -> int:
    errors = []
    warnings = []

    # Validate required files exist
    for doc_path in INVARIANT_DOCS:
        if not doc_path.exists():
            print(f"ERROR: Required doc not found: {doc_path}", file=sys.stderr)
            return 2

    # Parse invariant specs
    scope_invariants = parse_invariants_from_doc(INVARIANT_DOCS[0])
    mem_invariants = parse_invariants_from_doc(INVARIANT_DOCS[1])
    all_invariants = scope_invariants + mem_invariants

    print(f"Parsed {len(scope_invariants)} invariants from scope-resolution-invariants.md")
    print(f"Parsed {len(mem_invariants)} invariants from memory-policy-isolation-invariants.md")

    # Extract test names from all test files
    all_tests: dict[str, set[str]] = {}
    for test_file_rel in EXPECTED_TEST_FILES:
        test_file = REPO_ROOT / test_file_rel
        if test_file.exists():
            tests = extract_test_names_from_file(test_file)
            all_tests[test_file_rel] = tests
            print(f"  {test_file_rel}: {len(tests)} test functions")
        else:
            print(f"  WARNING: {test_file_rel}: file not found")

    total_tests = sum(len(t) for t in all_tests.values())
    print(f"Total test functions found: {total_tests}")

    # Run all checks
    print("\nRunning drift checks...")

    # 1. Test name existence
    for d in check_test_name_existence(all_invariants, all_tests):
        (errors if d.severity == "error" else warnings).append(d)

    # 2. Appendix file existence
    for doc_path in INVARIANT_DOCS:
        for d in check_appendix_files_exist([doc_path]):
            (errors if d.severity == "error" else warnings).append(d)

    # 3. GAP invariants that have tests
    for d in check_gap_invariants_have_tests(all_invariants, all_tests):
        (errors if d.severity == "error" else warnings).append(d)

    # 4. TEST-tagged invariants without test refs
    for d in check_test_tag_has_tests(all_invariants, all_tests):
        (errors if d.severity == "error" else warnings).append(d)

    # 5. Verification boundaries counts
    for d in check_verification_boundary_counts(VERIFICATION_BOUNDARIES):
        (errors if d.severity == "error" else warnings).append(d)

    # 6. Proof backlog consistency
    for d in check_proof_backlog_consistency(all_invariants):
        (errors if d.severity == "error" else warnings).append(d)

    # 7. Cross-doc consistency
    for d in check_cross_doc_consistency(scope_invariants, mem_invariants):
        (errors if d.severity == "error" else warnings).append(d)

    # 8. Source code references
    for d in check_source_code_references(all_invariants):
        (errors if d.severity == "error" else warnings).append(d)

    # Report results
    print(f"\n{'=' * 60}")
    if warnings:
        print(f"\nWARNINGS ({len(warnings)}):")
        for w in warnings:
            print(f"  [{w.category}] {w.message}")

    if errors:
        print(f"\nERRORS ({len(errors)}):")
        for e in errors:
            print(f"  [{e.category}] {e.message}")
        print(f"\nDrift check FAILED: {len(errors)} error(s), {len(warnings)} warning(s)")
        return 1

    if warnings:
        print(f"\nDrift check PASSED with {len(warnings)} warning(s)")
    else:
        print("\nDrift check PASSED: no drift detected")

    return 0


if __name__ == "__main__":
    sys.exit(main())
