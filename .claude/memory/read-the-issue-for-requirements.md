---
name: read-the-issue-for-requirements
description: When the intended behaviour is in doubt, trace commit → PR → linked issue and read the spec there — do not reason it out from the code
metadata:
  type: feedback
---

User rule (2026-08-25, made a hard rule in the `process-backlog` skill): whenever there is doubt
about what code is *supposed* to do, trace the PR and read the requirement in its linked issue.

The trail: `gh api repos/TIP-Global-Health/eheza-app/commits/<sha>/pulls` → the PR → its body's
issue reference → `gh issue view <n>`. E-Heza feature work carries a written, step-by-step clinical
spec in the issue; `gh pr view N --json body` here is usually just `#<issue>`.

**Why:** on B-235 I traced an inverted Healthy Start weight-gain verdict to a deliberate one-line
flip and framed it to the user as needing a product decision (one-sided vs two-sided band). Issue
#1604 stated the rule three times in plain words. There was no decision to make.

**How to apply:** read it BEFORE presenting a fix direction, not after. And read it before deciding
a fix is "revert the bad commit" — #1604 said adequate is *"equal to or greater than"* expected,
while the pre-flip code used strict `>`, so the revert would have shipped a second defect at the
boundary. Pin the boundary the spec names in a test. Related: [[verify-by-running-not-reasoning]],
[[pr-first-review-workflow]].
