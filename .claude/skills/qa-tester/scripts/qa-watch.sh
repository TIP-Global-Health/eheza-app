#!/bin/bash
# The GitHub and environment side of the QA watch (`/loop /qa-tester watch`).
#
#   bash .claude/skills/qa-tester/scripts/qa-watch.sh next
#       prints "<pr> <label>" for the PR to run next, or nothing
#   bash .claude/skills/qa-tester/scripts/qa-watch.sh ready
#       prints what stops a run on this station, or "ready"
#   bash .claude/skills/qa-tester/scripts/qa-watch.sh label <pr> ready|running|approved|followup|blocked
#       gives the PR that QA label and removes the other QA labels

set -eu

repo=TIP-Global-Health/eheza-app
main=$(git -C "$(dirname "$0")" worktree list --porcelain | sed -n '1s/^worktree //p')

case ${1:-} in
  next)
    # A PR still labelled QA Running had its run end without a result, so it goes first.
    for l in 'QA Running' 'Ready for QA'; do
      n=$(gh pr list -R "$repo" --state open --label "$l" --json number \
        --jq 'map(.number) | sort | .[0] // empty')
      if [ -n "$n" ]; then echo "$n $l"; exit 0; fi
    done
    ;;

  ready)
    problems=()
    branch=$(git -C "$main" branch --show-current)
    [ "$branch" = develop ] || problems+=("the main tree is on '$branch', not develop")
    staged=$(git -C "$main" diff --cached --name-only)
    [ -z "$staged" ] || problems+=("the main tree has staged files: $staged")
    port=$(sed -n 's/.*- \([0-9]*\):3000.*/\1/p' "$main/.ddev/docker-compose.client-http.yaml" 2>/dev/null | head -1)
    code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "http://localhost:${port:-3000}/Main.js" || true)
    [ "$code" = 200 ] || problems+=("gulp does not serve Main.js on port ${port:-3000} (HTTP $code)")
    if [ ${#problems[@]} -eq 0 ]; then echo ready; else printf '%s\n' "${problems[@]}"; fi
    ;;

  label)
    pr=${2:?usage: $0 label <pr> ready|running|approved|followup|blocked}
    case ${3:-} in
      ready) want='Ready for QA' ;;
      running) want='QA Running' ;;
      approved) want='Approved by QA' ;;
      followup) want='QA Followup' ;;
      blocked) want='QA Blocked' ;;
      *) echo "unknown QA state: ${3:-}" >&2; exit 1 ;;
    esac
    have=$(gh api "repos/$repo/issues/$pr/labels" --jq '.[].name')
    while IFS= read -r l; do
      case $l in
        'Ready for QA' | 'QA Running' | 'Approved by QA' | 'QA Followup' | 'QA Blocked') ;;
        *) continue ;;
      esac
      [ "$l" = "$want" ] && continue
      gh api -X DELETE "repos/$repo/issues/$pr/labels/$(jq -rn --arg l "$l" '$l | @uri')" >/dev/null
    done <<<"$have"
    gh api "repos/$repo/issues/$pr/labels" -f "labels[]=$want" --jq '[.[].name] | join(", ")'
    ;;

  *)
    sed -n '3,9p' "$0" >&2
    exit 1
    ;;
esac
