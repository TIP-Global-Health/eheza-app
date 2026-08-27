#!/bin/bash
# Create a durable worktree for one backlog item.
#
#   .claude/scripts/new-worktree.sh <branch-name>
#
# Sessions run in parallel, so worktrees live outside any session's scratchpad
# and stay until the PR merges. The main tree is never switched: it is parked on
# develop and only donates the heavy build inputs through symlinks.

set -eu

# Derived, not hardcoded: this script is tracked and travels between stations.
# It must be run from a checkout of the repo (the main tree or any worktree).
MAIN=$(git -C "$(dirname "$0")" rev-parse --path-format=absolute --show-toplevel)
ROOT="$MAIN-wt"

branch=${1:-}
if [ -z "$branch" ]; then
  echo "usage: $0 <branch-name>" >&2
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

git fetch -q origin develop
mkdir -p "$ROOT"
git worktree add "$wt" -b "$branch" origin/develop

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
echo "branch:   $branch (from origin/develop at $(git rev-parse --short origin/develop))"
echo "remove it when the PR merges:  git worktree remove $wt"
