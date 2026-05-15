#!/usr/bin/env bash
# Single workflow script for src-errormsg-validators:
#
#   1. Generate <case>/<variant>.hs by `cp`-ing the upstream validator from
#      src/ and applying <case>/<variant>.patch on top of it.
#   2. Compile the generated <variant>.hs with `cabal exec ghc -- -Wall
#      -fforce-recomp -c …` and capture the diagnostic output.
#   3. Filter the cabal/nix preamble out of the captured output and write
#      the result to <case>/<variant>.err next to the source file.
#
# Usage:
#   src-errormsg-validators/run.sh                # process every case directory
#   src-errormsg-validators/run.sh E04 E18        # only matching directories
#
# Requires the project's Nix dev shell. The compiler is the one cabal
# resolves for the project (GHC 9.6.6, plutus-tx-plugin 1.64.0.0,
# plutarch 1.10.x). GHC is expected to fail on most cases — this script
# always exits 0 unless an unexpected `patch` or shell error happens.

set -u

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ERROR_ROOT="$REPO_ROOT/src-errormsg-validators"

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

# 1. Generate: cp upstream validator + patch -p1
apply_patch() {
  local patch_file="$1"
  local out_hs="${patch_file%.patch}.hs"

  local src
  src=$(awk '/^# source:/ { sub(/^# source:[[:space:]]*/, ""); print; exit }' "$patch_file")
  if [ -z "$src" ]; then
    echo "  ! no '# source:' header in $patch_file" >&2
    return 1
  fi

  local src_full="$REPO_ROOT/$src"
  if [ ! -f "$src_full" ]; then
    echo "  ! source $src_full not found" >&2
    return 1
  fi

  cp "$src_full" "$out_hs"
  patch --quiet -p1 -d "$(dirname "$out_hs")" -i "$patch_file"
}

# 2 + 3. Compile and capture the filtered diagnostic.
compile_and_capture() {
  local file="$1"
  local err_out="$2"
  local case_dir
  case_dir="$(dirname "$file")"
  local raw="$case_dir/.${file##*/}.raw"

  (
    cd "$REPO_ROOT" || exit 1
    # `cabal exec` puts the project's package DB on GHC's search path so
    # `import Vesting.Types.VestingState` (etc.) resolves to the built
    # library. The default-extensions list mirrors the project's
    # `project-config` cabal stanza so the standalone files compile under
    # the same language semantics as the real validators.
    cabal exec ghc -- \
      -Wall \
      -fforce-recomp \
      -ferror-spans \
      -c \
      -package plinth-plutarch-paper-code \
      -odir "$case_dir/.build" \
      -hidir "$case_dir/.build" \
      -XBangPatterns -XDataKinds -XDeriveAnyClass -XDeriveGeneric \
      -XDerivingStrategies -XDerivingVia -XFlexibleInstances \
      -XGeneralizedNewtypeDeriving -XImportQualifiedPost -XInstanceSigs \
      -XLambdaCase -XMultiWayIf -XNumericUnderscores -XOverloadedRecordDot \
      -XOverloadedStrings -XQualifiedDo -XRankNTypes -XScopedTypeVariables \
      -XStandaloneDeriving -XTypeApplications -XTypeFamilies \
      -XTypeFamilyDependencies -XTypeOperators -XUndecidableInstances \
      -XViewPatterns \
      "$file"
  ) >"$raw" 2>&1
  local rc=$?

  # The Plinth plugin emits SGR escapes around its caret underlines even
  # when -fdiagnostics-color=never is passed; strip them so .err files
  # stay diffable.
  sed -E \
    -e '/^Using saved setting/d' \
    -e '/^Configuration is affected by/d' \
    -e "/^'\\/Users.*cabal.project'/d" \
    -e '/^Loaded package environment from/d' \
    -e '/^warning: Git tree/d' \
    -e $'s/\x1b\\[[0-9;]*[A-Za-z]//g' \
    "$raw" > "$err_out"
  rm -f "$raw"

  return $rc
}

# Classify what `compile_and_capture` produced.
verdict() {
  local rc="$1"
  local err="$2"
  if [ "$rc" -ne 0 ]; then
    echo "errors captured"
  elif grep -q 'warning:\|Compilation Error:' "$err"; then
    echo "warnings captured"
  elif [ -s "$err" ]; then
    echo "diagnostics captured"
  else
    echo "clean (no diagnostics)"
  fi
}

process_case() {
  local case_dir="$1"
  local name
  name="$(basename "$case_dir")"
  select_matches "$name" || return 0

  for variant in Plinth Plutarch; do
    local patch_file="$case_dir/$variant.patch"
    [ -f "$patch_file" ] || continue

    local hs="$case_dir/$variant.hs"
    local err="$case_dir/$variant.err"

    printf '%-40s %s.hs ' "$name" "$variant"

    if ! apply_patch "$patch_file"; then
      printf '... patch failed\n'
      continue
    fi
    printf '... '

    local rc=0
    compile_and_capture "$hs" "$err" || rc=$?
    printf '%s\n' "$(verdict "$rc" "$err")"
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
