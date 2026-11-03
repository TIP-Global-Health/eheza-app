---
name: rollbar-quiet-since-2026-09-18
description: "Rollbar Site-Rwanda went silent after 2026-09-18; looks like a natural tail-off, not a break; the live client token is valid (probed 2026-10-15); Burundi quiet since Sep 14 too"
metadata:
  node_type: memory
  type: project
  originSessionId: 29329e1d-299a-43ea-be41-a59bf44e1734
  modified: 2026-10-15T18:01:48.736Z
---

Checked 2026-10-15. Account usage was 1–6 per day in Aug 2026, about 1 every 3 days in Sep (last on Sep 18, item #24 "UUID already used"), and 0 since then. The volume shrank over weeks; it did not stop suddenly.

- Last live deploy was v1.18.2 on 2026-08-25. Occurrences kept arriving until Sep 18, so that build reports, and the `hedley_general_rollbar_token` it reads survived the deploy. PR #2031 deleted only `logs_rollbar_*` vars.
- The account is not over quota. The plan changed to Essentials (25k a month) on 2026-10-08.
- Live `hedley_general_rollbar_token` is set and Rollbar accepts it. An empty browser-platform POST gets 422 "data.body is required", which means it passed the token check. A bogus token gets 403 "not found". The token has `post_client_item` scope only.
- Not proven: no real test occurrence has been sent, so it is unconfirmed which project the token feeds. Reading prod vars is classifier-blocked, so the user runs `terminus ... vget` themselves.
- Site-Burundi (prj 662688) is in the same account and quiet too: last item Sep 14 (#11, person decode), the rest last seen around Aug 25. The items URL keeps only the first `prj=`, so open one project per URL.

Related: [[rollbar-postdeploy-triage-2026-07]]
