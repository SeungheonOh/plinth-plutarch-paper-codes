#!/usr/bin/env bash
# Compile every Plinth.hs / Plutarch.hs under src-errormsg/E*/ and
# capture the GHC diagnostic output to a sibling .err file.
#
# Usage:
#   ./collect-errors.sh            # process every error directory
#   ./collect-errors.sh E04 E18    # process only matching directories
#
# Requires the project's Nix dev shell. The compiler is the one cabal
# resolves for the project (currently GHC 9.6.6 with plutus-tx-plugin
# 1.64.0.0 / plutarch 1.10.x). Errors are expected — the script exits 0
# even when ghc fails.

set -u

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ERROR_ROOT="$REPO_ROOT/src-errormsg"

# Selector for which case directories to process. Empty means "all".
declare -a SELECT=()
for arg in "$@"; do
  SELECT+=("$arg")
done

select_matches() {
  local name="$1"
  if [ "${#SELECT[@]}" -eq 0 ]; then
    return 0
  fi
  for needle in "${SELECT[@]}"; do
    if [[ "$name" == *"$needle"* ]]; then
      return 0
    fi
  done
  return 1
}

compile_one() {
  local file="$1"
  local err_out="$2"
  local case_dir
  case_dir="$(dirname "$file")"
  (
    cd "$REPO_ROOT" || exit 1
    # Multiple plutus-tx-plugin-1.64.0.0 instances live in the cabal
    # store with the same (name, version) but different unit IDs. With
    # only `-package plutus-tx-plugin` GHC reports an ambiguity for the
    # `Plinth.Plugin` module that the toys load via `-fplugin`. Pull the
    # exact unit-id cabal pinned for this project out of the env file
    # and pass `-package-id` to disambiguate.
    cabal exec -- bash -c '
      env_file="$GHC_ENVIRONMENT"
      ptpid=$(awk "/^package-id plts-tx-plgn-/ {print \$2; exit}" "$env_file")
      ghc -Wall -fforce-recomp -c \
        -package plinth-plutarch-paper-code \
        ${ptpid:+-package-id "$ptpid"} \
        -odir "'"$case_dir"'/.build" \
        -hidir "'"$case_dir"'/.build" \
        "'"$file"'"
    '
  ) >"$err_out" 2>&1
  return $?
}

filter_log() {
  local in="$1"
  local out="$2"
  sed -E \
    -e '/^Using saved setting/d' \
    -e '/^Configuration is affected by/d' \
    -e "/^'\\/Users.*cabal.project'/d" \
    -e '/^Loaded package environment from/d' \
    -e '/^warning: Git tree/d' \
    "$in" > "$out"
}

process_case() {
  local case_dir="$1"
  local name
  name="$(basename "$case_dir")"
  select_matches "$name" || return 0

  for variant in Plinth Plutarch; do
    local src="$case_dir/$variant.hs"
    [ -f "$src" ] || continue

    local raw="$case_dir/.${variant}.raw"
    local err="$case_dir/${variant}.err"
    printf 'compile %-40s %s.hs ... ' "$name" "$variant"
    local rc=0
    compile_one "$src" "$raw" || rc=$?
    filter_log "$raw" "$err"
    rm -f "$raw"

    if [ $rc -ne 0 ]; then
      printf 'errors captured\n'
    elif grep -q 'warning:\|Compilation Error:' "$err"; then
      printf 'warnings captured\n'
    elif [ -s "$err" ]; then
      printf 'diagnostics captured\n'
    else
      printf 'clean (no diagnostics)\n'
    fi
  done
}

main() {
  shopt -s nullglob
  for case_dir in "$ERROR_ROOT"/E*; do
    [ -d "$case_dir" ] || continue
    process_case "$case_dir"
  done
}

main
