#!/bin/bash
# Check that the blind tests fail on the seam commit.
#
#   .claude/scripts/fail-first.sh <seam-ref> [client/src/elm/.../Test.elm ...]
#
# Run from the fix worktree. Builds a throwaway worktree at <seam-ref>, copies
# this tree's test files into it (the ones named, or every Test.elm and
# TestFixtures.elm that differs from <seam-ref>, committed or not) and runs them
# there. The gate passes only when they FAIL: a test that passes on the seam does
# not pin the defect, and one that does not compile depends on code the seam
# commit does not have.

set -eu

seam=${1:-}
if [ -z "$seam" ]; then
  echo "usage: $0 <seam-ref> [Test.elm paths relative to the repo root ...]" >&2
  exit 2
fi
shift

WT=$(git rev-parse --path-format=absolute --show-toplevel)
MAIN=$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")
cd "$WT"
seam_sha=$(git rev-parse --verify --quiet "$seam^{commit}") || {
  echo "fail-first: '$seam' is not a commit here." >&2
  exit 2
}

# Files to carry over: named ones, or everything test-shaped that differs.
if [ $# -gt 0 ]; then
  copy=("$@")
else
  mapfile -t copy < <(
    {
      git diff --name-only "$seam_sha" -- \
        ':(glob)client/src/elm/**/Test.elm' 'client/src/elm/TestFixtures.elm'
      git ls-files --others --exclude-standard -- ':(glob)client/src/elm/**/Test.elm'
    } | sort -u
  )
fi
run=()
for f in "${copy[@]}"; do
  [ -f "$f" ] || { echo "fail-first: $f is not a file in this tree." >&2; exit 2; }
  case "$f" in */Test.elm) run+=("${f#client/}") ;; esac
done
if [ ${#run[@]} -eq 0 ]; then
  echo "fail-first: no Test.elm differs from $(git rev-parse --short "$seam_sha") - nothing to check." >&2
  exit 2
fi

specs=$(git diff --name-only "$seam_sha" -- 'client/e2e/*.spec.ts'; git ls-files --others --exclude-standard -- 'client/e2e/*.spec.ts')
if [ -n "$specs" ]; then
  echo "fail-first: e2e specs also differ and are NOT checked here (they need the shared app):"
  printf '  %s\n' $specs
fi

tmp="$MAIN-wt/fail-first-$$"
mkdir -p "$MAIN-wt"
git worktree add -q --detach "$tmp" "$seam_sha"
# shellcheck disable=SC2064
trap "cd /; git -C '$WT' worktree remove --force '$tmp' >/dev/null 2>&1 || true" EXIT

# Same build inputs as new-worktree.sh; elm-stuff must be its own directory.
ln -s "$MAIN/client/node_modules"            "$tmp/client/node_modules"
ln -s "$MAIN/client/src/generated"           "$tmp/client/src/generated"
ln -s "$MAIN/client/src/elm/LocalConfig.elm" "$tmp/client/src/elm/LocalConfig.elm"
mkdir -p "$tmp/client/elm-stuff"
for f in "${copy[@]}"; do
  mkdir -p "$tmp/$(dirname "$f")"
  cp "$f" "$tmp/$f"
done

echo "fail-first: running ${#run[@]} test file(s) against $(git rev-parse --short "$seam_sha") ..."
set +e
out=$(cd "$tmp/client" && ./node_modules/.bin/elm-test "${run[@]}" 2>&1)
status=$?
set -e

if printf '%s\n' "$out" | grep -q 'TEST RUN FAILED'; then
  echo
  printf '%s\n' "$out" | grep -E '^(↓|✗)' || true
  echo
  printf '%s\n' "$out" | grep -E '^(Passed|Failed):'
  echo
  echo "fail-first: OK - the tests fail on the seam commit."
  echo "  (passing tests are pre-existing ones or guards - compare with the author's table)"
  exit 0
elif printf '%s\n' "$out" | grep -q 'TEST RUN PASSED'; then
  echo
  printf '%s\n' "$out" | grep -E '^(Passed|Failed):'
  echo
  echo "fail-first: NOT DISCRIMINATING - every test passes on the seam commit."
  echo "  The tests do not pin the defect; the criteria or the tests need another look."
  exit 1
else
  echo
  printf '%s\n' "$out" | tail -40
  echo
  echo "fail-first: DID NOT COMPILE against the seam commit (elm-test exit $status)."
  echo "  The tests depend on code the seam does not have - extend the seam commit, not the tests."
  exit 1
fi
