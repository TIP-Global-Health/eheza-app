#!/bin/bash
# Create a durable worktree for one backlog item.
#
#   .claude/scripts/new-worktree.sh <branch-name> [base-ref]
#
# The branch starts from origin/develop, or from <base-ref> when given: the
# blind test author starts from the seam commit of the fix branch.
#
# Sessions run in parallel, so worktrees live outside any session's scratchpad.
# A worktree lasts one round of work: release it once the work is pushed and
# recreate it here when more is needed. The main tree is never switched: it is
# parked on develop and only donates the heavy build inputs through symlinks.

set -eu

# Derived, not hardcoded: this script is tracked and travels between stations.
# It must be run from a checkout of the repo (the main tree or any worktree).
MAIN=$(dirname "$(git -C "$(dirname "$0")" rev-parse --path-format=absolute --git-common-dir)")
ROOT="$MAIN-wt"

branch=${1:-}
base=${2:-}
if [ -z "$branch" ]; then
  echo "usage: $0 <branch-name> [base-ref]" >&2
  exit 1
fi
wt="$ROOT/$branch"

cd "$MAIN"
git worktree prune
if git worktree list --porcelain | grep -qx "branch refs/heads/$branch"; then
  echo "'$branch' is already checked out - another session may be working it:" >&2
  git worktree list >&2
  exit 1
fi
[ -e "$wt" ] && { echo "$wt already exists." >&2; exit 1; }

if [ -n "$base" ]; then
  git fetch -q origin
else
  git fetch -q origin develop
  base=origin/develop
fi
mkdir -p "$ROOT"
# A released worktree leaves its branch behind; re-attach to it rather than
# failing on `-b`, so a second round of work on the same branch is one command.
if git show-ref --verify --quiet "refs/heads/$branch"; then
  git worktree add "$wt" "$branch"
else
  git worktree add "$wt" -b "$branch" "$base"
fi

# Build inputs the main tree already has. elm-stuff is deliberately NOT
# symlinked: elm-test writes a generated project whose relative
# source-directories would resolve back through the link into the main tree, so
# every compile and every test would silently run against the main tree's
# sources.
ln -s "$MAIN/client/node_modules"            "$wt/client/node_modules"
ln -s "$MAIN/client/src/generated"           "$wt/client/src/generated"
ln -s "$MAIN/client/src/elm/LocalConfig.elm" "$wt/client/src/elm/LocalConfig.elm"
mkdir -p "$wt/client/elm-stuff"

echo
echo "worktree: $wt"
echo "branch:   $branch (from $base at $(git rev-parse --short "$base"))"
echo "release it once the work is pushed:  git worktree remove $wt"
