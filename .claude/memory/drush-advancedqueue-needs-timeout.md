---
name: drush-advancedqueue-needs-timeout
description: "`drush advancedqueue <queue>` loops forever waiting for items; always pass --timeout=N locally (Jenkins uses --all --timeout=600)"
metadata:
  type: feedback
---

`ddev drush advancedqueue <queue-name>` processes the queue and then keeps polling for new items
until `--timeout` elapses; without the flag it never returns and the Bash call hangs.

**Why:** hit on 2026-10-19 while verifying the HealthyStart dashboard queue (PR #2379); the
worker had already run, the command just never exited. Killing needs the in-container PID
(`ddev exec pgrep -af advancedqueue`, then `ddev exec kill <pid>`), the host `ddev` client alone
does not stop it.

**How to apply:** `ddev drush advancedqueue <queue> --timeout=5` for a one-shot drain. Related:
[[prod-advancedqueue-jenkins-serialized]].
