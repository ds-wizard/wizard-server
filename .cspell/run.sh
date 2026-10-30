#!/usr/bin/env sh
#
# Runs CSpell over the spell checked file types. CI installs the cspell-cli
# package; a plain cspell installation or npx works the same.
#
# The globs are quoted so that CSpell expands them recursively itself, and
# --gitignore keeps the untracked local config files out of the run.

if command -v cspell-cli > /dev/null 2>&1; then
    CSPELL=cspell-cli
elif command -v cspell > /dev/null 2>&1; then
    CSPELL=cspell
else
    CSPELL="npx --yes cspell-cli"
fi

$CSPELL "$@" \
    --config .cspell/cspell.json \
    --gitignore \
    "**/*.hs" \
    "**/*.md" \
    "**/*.json" \
    "**/*.toml" \
    "**/*.yml" \
    "**/*.yaml"
