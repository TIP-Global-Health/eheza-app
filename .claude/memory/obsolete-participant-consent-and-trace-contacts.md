---
name: obsolete-participant-consent-and-trace-contacts
description: participant_consent and acute_illness_trace_contact are obsolete features - do not design around their data shapes
metadata:
  type: project
---

The user, 2026-08-27: *"they're both obsolete"* — `participant_consent` (group session consent
forms) and `acute_illness_trace_contact` (COVID contact tracing).

**Why:** both are still fully wired in the repo — Elm types, encoders, Drupal bundles, feature
exports — so the code gives no sign they are dead. Live confirms it: zero nodes of either on all
four sites (ihangane, vhw, uvl, tip-somalia, checked 2026-08-27).

**How to apply:** do not add special handling for their data shapes. Both have a legitimate
many-per-encounter shape that would otherwise need exemptions from uniqueness rules — a consent per
form, a node per traced contact — and carrying those exemptions is not worth the complexity. They
came up while keying the duplicate sweep and the client write guard; see [[live-queries-must-filter-field-deleted]].
