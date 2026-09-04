#!/usr/bin/env bash

set -euo pipefail

git log --format='commit %cd' --date=format:%Y%m%d%H%M.%S --name-only |
  awk '/^commit / { time = $2; next } NF && !seen[$0]++ { print time "\t" $0 }' |
  while IFS=$'\t' read -r time file; do
    if [ -f "$file" ]; then
      touch -m -t "$time" "$file"
    fi
  done
