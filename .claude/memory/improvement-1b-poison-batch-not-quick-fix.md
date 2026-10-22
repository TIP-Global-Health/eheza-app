---
name: improvement-1b-poison-batch-not-quick-fix
description: Code-review improvement
metadata: 
  node_type: memory
  type: project
  originSessionId: 3a0f2003-dc62-444e-8130-8870758fa415
---

From the June 2026 E-Heza code-review sweep, proposal #1b was the "poison batch" sync bug: in `SyncManager`, one server-rejected record permanently jams its own health-center (shard) upload queue — its ~49 all-or-nothing batch-mates roll back with it, and everything created later (higher localId, FIFO, no offset) never gets its turn. Silent: failures fall to `_ -> noChange`.

**Do NOT propose #1b as a quick "skip/quarantine the bad node" fix — that change is invalid.** The user established this and pushed back when I mis-listed it as a parked quick-fix.

**Why the naive client-side skip is invalid:** uploads are FIFO and content carries mandatory backend FK references (participant→person, encounter→participant, measurement→encounter). A node's legitimate dependents **cannot be uploaded ahead of it**, so you can't "drain the rest" past a skipped node — much of "the rest" depends on it and would fail FK validation. The client also can't identify *which* record is poison: the server batch is all-or-nothing with no per-record outcome reported.

**Only valid directions:**
- **Option A** — make the jam visible (surface a sync incident / alert), skip nothing, lose nothing. Overlaps proposal #2 (sync-trust UX visibility). UX change only.
- **Option C** — backend change: commit independent records per-item and report per-record failures, so only the genuinely-dependent chain blocks.

Status: deferred — user said "skip 1b for now" (not abandoned). It is NOT a small client-side fix. Related: proposal #2 (sync status visibility) which Option A folds into.

2026-07-06: full design briefs now exist for both directions — [[design-brief-sync-jam-visibility]] (Option A) and [[design-brief-backend-per-record-commit]] (Option C, incl. the mandatory capability-flag rollout to avoid old-client data loss). Facts re-verified on develop @91fb401fb.
