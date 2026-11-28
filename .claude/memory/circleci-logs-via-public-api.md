---
name: circleci-logs-via-public-api
description: "CircleCI job steps and logs for eheza-app are readable with no token via the public v2/v1.1 API; e2e \"Install Drupal\" normally takes ~400 s, so a 10-min silent timeout there is an infra hang"
metadata:
  node_type: memory
  type: reference
  originSessionId: ffbbba47-6897-4f8a-a3af-875ce8f2c04b
  modified: 2026-11-23T12:15:12.477Z
---

eheza-app is public on CircleCI, so no token is needed to read CI. Rerunning a job does need one (none on this station), so a rerun means the CircleCI UI or an empty commit.

- Jobs of a workflow: `curl -s https://circleci.com/api/v2/workflow/<wf-id>/job` (the wf id is in the `gh pr checks` URL).
- Steps of a job: `curl -s https://circleci.com/api/v1.1/project/github/TIP-Global-Health/eheza-app/<job_number>`. Each step has `status` and `run_time_millis`. A failed step's `output_url` returns the log as JSON messages.

Baseline (2026-11-23): e2e "Install Drupal" takes 400–415 s in every e2e job. On #2577, one job hung silently after the DDEV image pull and hit "Too long with no output (exceeded 10m0s)" while its sibling jobs installed in ~410 s. That is environment, not code.
