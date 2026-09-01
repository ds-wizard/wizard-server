#!/usr/bin/env bash
#
# Builds the library, executables and test suites and fails when GHC emitted
# any warning. This is the build gate the CI pipeline runs; `make build-strict`
# runs the very same script locally.
#
# Usage: ./scripts/build-strict.sh [extra stack build arguments]
# Note that GHC only warns about modules it actually recompiles, so add
# `--ghc-options -fforce-recomp` (locally: `make build-strict RECOMP=1`) to see
# the warnings of modules that are already up to date.

set -euo pipefail
cd "$(dirname "$0")/.."

LOG=$(mktemp)
GHC_LOG=$(mktemp)
trap 'rm -f "$LOG" "$GHC_LOG"' EXIT

stack build wizard-server --test --no-run-tests "$@" 2>&1 | tee "$LOG"

# The linker warnings of the macOS jinja workaround are not GHC diagnostics, and stack prefixes
# the output of a dependency it has to build with "<package> > " - those warnings are not ours.
grep -vE '^(ld: |[A-Za-z0-9_.-]+ *> )' "$LOG" > "$GHC_LOG" || true

WARNINGS=$(grep -c ': warning: ' "$GHC_LOG" || true)
if [ "$WARNINGS" -gt 0 ]; then
  echo "!! GHC WARNINGS DETECTED ($WARNINGS):"
  grep ': warning: ' -A 3 "$GHC_LOG"
  exit 1
fi
