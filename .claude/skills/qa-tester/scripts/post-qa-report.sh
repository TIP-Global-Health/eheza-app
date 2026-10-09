#!/bin/bash
# Post a drafted QA report as a comment on the PR and print the comment's URL.
#
#   bash .claude/skills/qa-tester/scripts/post-qa-report.sh <pr> <report.md>
#
# Uses the REST API because `gh pr comment` fails on this repo. Refuses a report
# that does not open with the QA caption, so a stray file is never posted.

set -eu

pr=${1:?usage: $0 <pr> <report.md>}
file=${2:?usage: $0 <pr> <report.md>}
caption='**Manual tests executed using the QA Tester skill**'

[ -s "$file" ] || { echo "empty or missing: $file" >&2; exit 1; }
first=$(grep -m1 -v '^[[:space:]]*$' "$file" | sed 's/^> *//')
if [ "$first" != "$caption" ]; then
  echo "the report must open with: $caption" >&2
  exit 1
fi

gh api "repos/TIP-Global-Health/eheza-app/issues/$pr/comments" --method POST \
  -f body="$(cat "$file")" --jq '.html_url'
