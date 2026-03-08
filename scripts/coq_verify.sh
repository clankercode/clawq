#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

OPAM_SWITCH="${OPAM_SWITCH:-$(opam switch show 2>/dev/null || echo clawq-5.1)}"
COQC="opam exec --switch=${OPAM_SWITCH} -- coqc"

if ! opam exec --switch="${OPAM_SWITCH}" -- which coqc >/dev/null 2>&1; then
  echo "coqc is required. Run scripts/bootstrap_coq.sh first."
  exit 1
fi

echo "Compiling Coq theories (switch: ${OPAM_SWITCH})..."
# Core definitions
${COQC} -R coq/theories Clawq coq/theories/Clawq/Interfaces.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/Config.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/Cli.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/PathSafety.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/AuditChain.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/AuditChainConcrete.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/RateLimiter.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/QuoteParsing.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/ShellSafety.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/SecretStore.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/ChannelAuth.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/AuditRetention.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/AgentLoop.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/SessionIsolation.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/LandlockPolicy.v

echo "Compiling Coq proofs..."
${COQC} -R coq/theories Clawq coq/theories/Clawq/ConfigProofs.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/CliProofs.v

if rg -n '^Admitted\.$' coq/theories/Clawq/LandlockPolicy.v >/dev/null; then
  echo "LandlockPolicy.v still contains Admitted proofs."
  rg -n '^Admitted\.$' coq/theories/Clawq/LandlockPolicy.v
  exit 1
fi

echo "Verified: coq/theories/Clawq/LandlockPolicy.v contains no Admitted proofs."
