---
name: diagnosis-defects-are-tier-1
description: Standing tiering rule (user, 2026-09-03) — a defect in how a diagnosis or its care pathway is determined is tier 1 regardless of population; severity stays a blast-radius label
metadata:
  type: feedback
---

⛔ **A defect in how a DIAGNOSIS or its CARE PATHWAY is determined is TIER 1, regardless of how many patients hit it.** (user, 2026-09-03, on B-236: *"this is a bug when diagnosis is determined, which causes an illness not to be detected. That's top priority."*)

**Why:** tiers had been set by blast radius (population, routing-vs-detection), leaving MED-HIGH clinical-logic items (B-236, B-245) in tier 3 under a tier-1 definition that literally said HIGH/MED-HIGH. The user's priority is the clinical decision itself, not the count of people behind it.

**How to apply:** the rule covers the decision logic — which diagnosis fires, which treatment/referral/medication set it routes to (B-221, B-237, B-240, B-286, B-288, B-289, B-319, B-320 were re-tiered 3 → 1). It does NOT cover things that merely mention a diagnosis: report rows and mappings (B-133, B-227, B-245), case-management listings (B-287, B-306), form-refill families (B-274/B-281), display panes (B-322), wrong-task consequences (B-275). Keep the severity label as blast radius; the tier is priority. The rule is written into `queue.md`'s Tier 1 line. See [[worktree-per-item-for-parallel-sessions]] for the process context and [[meaningful-issue-and-pr-titles]] for how these ship.
