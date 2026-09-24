---
name: gh-pr-edit-projectcards-workaround
description: gh pr edit fails on the eheza-app repo with a Projects-classic GraphQL error; use gh api PATCH instead
metadata: 
  node_type: memory
  type: reference
  originSessionId: 3a0f2003-dc62-444e-8130-8870758fa415
---

On `TIP-Global-Health/eheza-app`, `gh pr edit <N> --title/--body` fails with:
`GraphQL: Projects (classic) is being deprecated ... (repository.pullRequest.projectCards)`
and does NOT apply the change (the title/body stay unchanged).

**Workaround** — update via the REST API, which never touches projectCards:
```
jq -n --arg t "TITLE" --rawfile b body.md '{title:$t, body:$b}' \
  | gh api repos/TIP-Global-Health/eheza-app/pulls/<N> -X PATCH --input -
```
`gh pr view --json`, `gh pr create`, and `gh issue create` all work fine; only `gh pr edit` is affected.
