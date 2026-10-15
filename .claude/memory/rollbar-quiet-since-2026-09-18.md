---
name: rollbar-quiet-since-2026-09-18
description: "Rollbar Site-Rwanda went silent after 2026-09-18; the evidence points to a natural tail-off, not a broken pipe, but an end-to-end test was never run; Site-Burundi project no longer visible"
metadata:
  node_type: memory
  type: project
  originSessionId: 29329e1d-299a-43ea-be41-a59bf44e1734
  modified: 2026-10-15T18:01:48.736Z
---

Checked 2026-10-15. Account usage was 1–6 per day in Aug 2026, about 1 every 3 days in Sep (last on Sep 18, item #24 "UUID already used"), and 0 since then. The volume shrank over weeks; it did not stop suddenly.

- Last live deploy was v1.18.2 on 2026-08-25. Occurrences kept arriving until Sep 18, so that build reports, and the `hedley_general_rollbar_token` it reads survived the deploy. PR #2031 deleted only `logs_rollbar_*` vars.
- The account is not over quota. The plan changed to Essentials (25k a month) on 2026-10-08.
- Not proven: no test occurrence has been sent with the live token. Reading that token on prod was blocked by the auto-mode classifier, so the user must run `terminus ... vget` themselves.
- Site-Burundi (prj 662688) no longer shows in the tipglobalhealth account. Only Site-Rwanda is listed.

Related: [[rollbar-postdeploy-triage-2026-07]]
