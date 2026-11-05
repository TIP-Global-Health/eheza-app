#!/bin/bash
# Post a drafted QA report as a comment on the PR and print the comment's URL.
#
#   bash .claude/skills/qa-tester/scripts/post-qa-report.sh <pr> <report.md> [--update <comment-id>]
#
# With --update, the run's earlier report comment is replaced instead of a new one added.
# Refuses a report that does not open with the QA caption, so a stray file is never posted,
# and one with no sentence on what the error channel showed on each build.

set -eu

pr=${1:?usage: $0 <pr> <report.md>}
file=${2:?usage: $0 <pr> <report.md> [--update <comment-id>]}
update=
[ "${3:-}" = "--update" ] && update=${4:?--update needs the comment id}
caption='**Manual tests executed using the QA Tester skill**'

[ -s "$file" ] || { echo "empty or missing: $file" >&2; exit 1; }
first=$(grep -m1 -v '^[[:space:]]*$' "$file")
if [ "$first" != "$caption" ]; then
  echo "the report's first line must be exactly, with no '>' in front: $caption" >&2
  exit 1
fi
# The error channel is read on both builds, so the report says what each showed.
if ! grep -iE 'error' "$file" | grep -qiE 'develop|either build|both builds|each build'; then
  echo "the report must say what the error channel showed on each build, e.g. 'No console errors, failed requests or error responses on either build'" >&2
  exit 1
fi

if [ -n "$update" ]; then
  gh api "repos/TIP-Global-Health/eheza-app/issues/comments/$update" --method PATCH \
    -f body="$(cat "$file")" --jq '.html_url'
else
  gh pr comment "$pr" --body-file "$file"
fi
