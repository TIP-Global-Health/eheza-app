#!/bin/bash
# Commit and push backlog bookkeeping when a turn ends.
#
# The backlog records what was decided and shipped. They were repeatedly
# written and left uncommitted, so the repository and the records disagreed.
# This commits them where they belong - on develop - and says so when it
# cannot.
#
# Never fails the turn: always exits 0.
#
# Sessions run in parallel against this one tree, so the commit and the push are
# serialized under a lock and the push rebases onto whatever another session
# pushed first.

set -u
cd "${CLAUDE_PROJECT_DIR:-/var/www/html/ihangane}" 2>/dev/null || exit 0

PATHS=(.claude/backlog .claude/memory .claude/skills .claude/scripts .claude/agents .claude/settings.json)

dirty=$(git status --porcelain -- "${PATHS[@]}" 2>/dev/null)
[ -z "$dirty" ] && exit 0

# Serialize against the other sessions' hooks. Without the lock two of them
# stage each other's half-written edits and race on the index.
lock="$(git rev-parse --git-common-dir 2>/dev/null)/backlog-bookkeeping.lock"
if [ -z "${BOOKKEEPING_LOCK_HELD:-}" ] && command -v flock >/dev/null 2>&1; then
  export BOOKKEEPING_LOCK_HELD=1
  exec flock -w 45 "$lock" "$0" "$@"
fi

branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
count=$(printf '%s\n' "$dirty" | grep -c .)

emit () { python3 -c 'import json,sys; print(json.dumps({"systemMessage": sys.argv[1]}))' "$1"; }

if [ "$branch" != "develop" ]; then
  emit "⚠ $count bookkeeping file(s) under .claude/ are uncommitted, and this tree is on '$branch'. Backlog records belong on develop - commit them there before switching away."
  exit 0
fi

# Name the items whose entries changed, so the message says what it recorded.
items=$(printf '%s\n' "$dirty" | sed -n 's#.* \.claude/backlog/items/\([A-Za-z0-9-]*\)\.md#\1#p' | sort -u | tr '\n' ' ')
[ -n "$items" ] && subject="Backlog: record ${items% }" || subject="Backlog: record the current state"

git add -- "${PATHS[@]}" 2>/dev/null || { emit "⚠ Could not stage bookkeeping files."; exit 0; }

# Nothing staged after add (e.g. only ignored paths matched).
git diff --cached --quiet -- "${PATHS[@]}" && exit 0

if ! git commit -q -m "$subject" -m "Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>" -m "[ci skip]" 2>/dev/null; then
  emit "⚠ Bookkeeping files are staged but the commit failed."
  exit 0
fi

sha=$(git rev-parse --short HEAD)

push () { git push -q origin develop 2>/dev/null; }

if push; then
  emit "📒 Bookkeeping committed and pushed: $sha - $subject ($count file(s))."
  exit 0
fi

# Another session pushed first. Rebase onto it and try once more; autostash
# keeps any unrelated edits in the tree intact.
if git -c rebase.autoStash=true pull --rebase -q origin develop 2>/dev/null && push; then
  emit "📒 Bookkeeping committed and pushed after rebasing onto another session's push: $(git rev-parse --short HEAD) - $subject ($count file(s))."
else
  git rebase --abort 2>/dev/null
  emit "📒 Bookkeeping committed as $sha but the push failed - pull --rebase and push develop when you can."
fi
exit 0
