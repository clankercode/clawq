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
${COQC} -R coq/theories Clawq coq/theories/Clawq/ToolSafety.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/PairCoding.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/PmodelParsing.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/TaskTree.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/SchedulerCron.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/DiscordGateway.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/SandboxPolicy.v

echo "Compiling Coq proofs..."
${COQC} -R coq/theories Clawq coq/theories/Clawq/ConfigProofs.v
${COQC} -R coq/theories Clawq coq/theories/Clawq/CliProofs.v

check_no_admitted() {
  local file="$1"
  local name
  name="$(basename "$file")"

  if grep -nH '^Admitted\.$' "$file"; then
    echo "${name} still contains Admitted proofs."
    exit 1
  fi

  echo "Verified: ${file} contains no Admitted proofs."
}

check_no_admitted coq/theories/Clawq/LandlockPolicy.v
check_no_admitted coq/theories/Clawq/PairCoding.v
check_no_admitted coq/theories/Clawq/PmodelParsing.v
check_no_admitted coq/theories/Clawq/TaskTree.v
check_no_admitted coq/theories/Clawq/SchedulerCron.v
check_no_admitted coq/theories/Clawq/DiscordGateway.v
check_no_admitted coq/theories/Clawq/SandboxPolicy.v
