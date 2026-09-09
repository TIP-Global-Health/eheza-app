---
name: enum-variant-breaks-deployed-clients
description: "⛔ Adding a variant to a client-decoded enum (list_text field) is a BREAKING change for devices already in the field — their strict decoder fails the whole download batch and the sync jams until the device updates; a new FIELD is safe, a new VALUE is not"
metadata: 
  node_type: memory
  type: project
  originSessionId: bdde38ba-5064-453d-a2da-3a8e13dc3509
  modified: 2026-09-09T13:16:25.397Z
---

Adding a new value to an existing enum that the client decodes — a Drupal `list_text`
allowed value with an Elm `xFromString` — cannot ship in one release. Learned on B-338
(PR #2228, 2026-09-09), where `BloodSmearNotRunAtLab` was added to `BloodSmearResult`.

**Why:** the Elm decoders are strict (`bloodSmearResultFromString` → `fail` on an unknown
string; `optional` only defaults on absent/null), and the authority download is decoded as a
plain `list` (`SyncManager/Decoder.elm` `decodeDownloadSyncResponseAuthority`). One record a
deployed device cannot decode fails its whole batch, the revision cursor never advances, and
that device re-fetches the same batch forever — the `obstetric_history_step2` jam shape. The
vulnerable code is already on the devices, so no edit to the new PR can protect them.

**How to apply:**
- ⛔ A new VALUE on a decoded enum needs a two-release rollout: ship a tolerant decoder
  (`decodeWithFallback <default> decodeX`, `Utils/Json.elm`) first, release, wait for device
  adoption, THEN ship the value. Adoption cannot be measured, so "wait" is a guess.
- ✅ A new FIELD is safe in one release: Elm's pipeline decoders ignore JSON keys they do not
  ask for, and old clients simply do not send it (absent → `optional` default).
- So when a record needs a new bit of information, prefer a new field over a new enum value.
- Hygiene worth doing anyway, as its own cheap PR: wrap the strict enum decoders in
  `decodeWithFallback`, so the NEXT extension is possible without a two-release wait.
- Related: [[improvement-1b-poison-batch-not-quick-fix]] (why the client cannot just skip the
  bad record), [[design-brief-backend-per-record-commit]] (the capability-flag rollout rule).
