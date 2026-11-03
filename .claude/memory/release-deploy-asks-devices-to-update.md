---
name: release-deploy-asks-devices-to-update
description: Every E-Heza release deploy already comes with a request to update all devices ASAP — don't propose release-note lines asking centres to update devices
metadata:
  type: feedback
---

Every release deploy already asks health centres to update all devices as soon as possible (user, 2026-10-21, on B-369's suggested "update every device together" release-note line).

**Why:** it is standing deploy practice, so a per-defect release-note line saying the same thing adds nothing.

**How to apply:** for mixed-fleet defects (old and new builds at one centre, see [[enum-variant-breaks-deployed-clients]]), size the risk over the normal adoption window and fix what can be fixed in code; don't offer "tell centres to update" as a mitigation.
