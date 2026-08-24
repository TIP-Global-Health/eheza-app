---
name: prod-advancedqueue-jenkins-serialized
description: Production Advanced Queue is driven by a Jenkins job with concurrent builds DISABLED → no two drush advancedqueue processes ever run against a site DB
metadata: 
  node_type: memory
  type: reference
  originSessionId: 06747026-1426-464c-8ff7-c8a48f18d81b
---

**Production Advanced Queue processing is SERIALIZED per site.** Established 2026-07-16 by reading the live Jenkins job config via browser automation (the user pointed me at it; the answer is not in the repo or the site DB).

- Job: `ci.gizra.com/view/Ihangane/job/ihangane.live__advanced_queue__1` (one per site; vhw/tip-somalia presumably cloned).
- **"Execute concurrent builds if necessary" = UNCHECKED** → Jenkins runs one build at a time; a new trigger QUEUES behind the running build, never runs alongside it.
- Schedule: `H/10 * * * *` (~every 10 min; the `H` is Jenkins hash syntax, confirming Jenkins, not plain cron). The repo's `infrastructure_setup/advancedqueue.sh` comment says `H/5` — STALE, the live job is H/10.
- Invocation: `drush @pantheon.eheza-app.live advancedqueue --all --timeout=300` (`--timeout=300` stops CLAIMING new items after 5 min, so a run finishes well inside the 10-min interval). No throttle / build-timeout plugin.

**Consequence for queue-race findings:** any bug that requires two concurrent AQ worker processes against the same site DB has ~zero live hit-rate. This is what closed [[improvement-backlog]] B-058 part 2 (NCDA+Reports cross-worker lost update) and B-059 (view_export FILE_APPEND parallel-claim) as WON'T-FIX. The ONLY residual concurrency source is a human running `drush advancedqueue` by hand while the cron is mid-run — not normal operation.

⚠ Single-worker bugs are UNAFFECTED by this — e.g. B-098/B-058-part-1 (a worker reverting concurrent PERSON EDITS via a stale full-node `node_save`) fire with ONE worker plus ordinary device sync, and WERE real/fixed. Don't over-apply "it's serialized" to dismiss single-worker races.

To re-verify the Jenkins setting: browser-automate to the job's `/configure` page (user must be logged into ci.gizra.com), then read the checkbox's real `.checked` property via javascript_tool — `find`/`read_page` infer state from `value="on"` which is the DEFAULT and unreliable.
