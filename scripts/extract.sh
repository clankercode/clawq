#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

OPAM_SWITCH="${OPAM_SWITCH:-clawq-5.1}"
COQC="opam exec --switch=${OPAM_SWITCH} -- coqc"

if ! opam exec --switch="${OPAM_SWITCH}" -- which coqc >/dev/null 2>&1; then
  echo "coqc is required. Run scripts/bootstrap_coq.sh first."
  exit 1
fi

echo "Compiling Coq theories (switch: ${OPAM_SWITCH})..."
# Core definitions (no inter-module dependencies)
${COQC} -R coq/theories Clawq coq/theories/Clawq/Interfaces.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/Config.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/Cli.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/PathSafety.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/AuditChain.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/RateLimiter.v

# Proof files (depend on core definitions; compiled for verification only)
${COQC} -R coq/theories Clawq coq/theories/Clawq/ConfigProofs.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/CliProofs.v

echo "Running extraction..."
${COQC} -R coq/theories Clawq coq/theories/Clawq/Extract.v

if [ ! -f src/extracted/clawq_core.ml ]; then
  echo "ERROR: extraction did not produce src/extracted/clawq_core.ml"
  exit 1
fi

echo "Extraction complete: src/extracted/clawq_core.ml src/extracted/clawq_core.mli"
