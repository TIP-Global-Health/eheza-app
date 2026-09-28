---
name: request-copilot-review-via-api
description: How to request a GitHub Copilot review on an eheza-app PR from the CLI (and verify it)
metadata: 
  node_type: memory
  type: reference
  originSessionId: a701beb6-1897-4e83-82ea-7cc14d8d6294
  modified: 2026-08-02T11:18:06.284Z
---

⛔ **Ask the user first — never request one automatically** (2026-08-02). The quota is small,
shared with all other Copilot use, and a blocked request posts a stub review that reads as a clean
pass. Full rule and the evidence: [[pr-first-review-workflow]].

To request a **Copilot review** on a TIP-Global-Health/eheza-app PR, POST the bot to the REST requested-reviewers endpoint (works; `gh pr edit --add-reviewer` does not — see [[gh-pr-edit-projectcards-workaround]]):

```
gh api -X POST repos/TIP-Global-Health/eheza-app/pulls/<N>/requested_reviewers \
  -f 'reviewers[]=copilot-pull-request-reviewer[bot]'
```

Copilot code review is enabled on this repo (used since round-1 PR #1788).

**Verify** with the REST endpoints, NOT `gh pr view --json reviewRequests` — that jq path returns blank for a Bot's login (Copilot shows as `type: Bot`, login `Copilot`, and doesn't surface there):
- pending: `gh api .../pulls/<N>/requested_reviewers -q '[.users[].login]|join(",")'` → shows `Copilot`
- submitted: `gh api .../pulls/<N>/reviews -q '[.[].user.login]|unique|join(",")'`

A reviewer drops out of `requested_reviewers` once they submit a review, so check both.
