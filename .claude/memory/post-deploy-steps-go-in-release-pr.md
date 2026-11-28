---
name: post-deploy-steps-go-in-release-pr
description: "A merged PR's post-deploy steps (drush scripts etc.) are added to the open develop→main release PR's \"Post deploy instructions\" section"
metadata:
  node_type: memory
  type: feedback
  originSessionId: ffefc507-d670-4f38-8798-1beb219fa38b
  modified: 2026-11-25T08:33:05.037Z
---

When a PR that needs post-deploy work merges into develop, add its entry to the open release PR's body (develop→main, "Developments starting …", #2417 as of 2026-11-25) under `### Post deploy instructions:`.

Format: `#<PR> (on each site …):` then indented `- ` lines with the full `drush scr profiles/hedley/...` path, flags, and why.

**Why:** the user pointed at that list after I only reported #2586's step in chat and HANDOFF (2026-11-25); the release is run from it.

**How to apply:** do it in the same turn as the merge. Edit the body via `gh api repos/TIP-Global-Health/eheza-app/pulls/N -X PATCH -F body=@file` ([[gh-pr-edit-projectcards-workaround]]); diff the live body first.
