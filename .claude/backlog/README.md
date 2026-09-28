# Improvement backlog

State for the `/process-backlog` and `/run-backlog-discovery` skills. The skills describe *how* to work; everything
here is *what has happened*. Nothing in this directory belongs in a skill file, and no skill file
should record a round number, a tier count or a cursor.

| file | what it is |
|---|---|
| `index.tsv` | derived index — one row per item: id, tier, status, severity, issue, PR, round, title |
| `items/<id>.md` | the full entry for one finding, as written by the round that found it |
| `queue.md` | the priority-queue header: tier lines, conventions, live gotchas |
| `coverage.md` | lens registry + module coverage table + declined classes + verification lessons |
| `rounds.md` | per-round sections and their notes/minors |
| `HANDOFF.md` | the live cursor — what is open, blocked, in flight |
| `archive/` | rounds 1–9 ledger, frozen |

## index.tsv is derived — never edit it

`./reindex.py` rebuilds it from `items/*.md`. Status, severity, issue and PR are read out of the
entry's own prose, so **updating an entry updates the index**. Only `tier` is stored, in the item's
`<!-- id: status: tier: round: -->` line, because it is a triage decision rather than something the
entry states.

Run `./reindex.py` after any batch of item edits.

## Querying

```bash
awk -F'\t' '$3=="READY"' index.tsv | wc -l              # how many are queued
awk -F'\t' '$3=="READY" && $2=="2"' index.tsv           # tier 2, ready
awk -F'\t' '$3=="PARKED" {print $1"\t"$8}' index.tsv    # never re-pitch these
cut -f3 index.tsv | sort | uniq -c | sort -rn           # status distribution
```

## Statuses

`READY` (verified, **not yet put to the user**) · `MONITORING` · `IMPLEMENTED` · `PARKED`
(user declined — never re-pitch) · `WONTFIX` · `REFUTED` · `CLOSED` · `SPLIT` · `SKIPPED` ·
`STALE` · `UNKNOWN` (the entry states no lifecycle status — treat as *not* ready and read it).

### MONITORING

**Something was shipped and the item is now waiting on evidence.** Instrumentation, a fix, or a
script is in place and the next move belongs to the field: a deploy, a Rollbar window, a data
sweep. There is nothing to build and nothing to ask.

An entry marked MONITORING must say two things, or it will rot: **what it is waiting for**, and
**what would end the wait**.

⚠ Not the same as *gated*. An item waiting on someone to answer a question — ops, a clinician,
the user — is still actionable today: the action is to ask. Those stay READY and say so in their
prose. MONITORING is only for waiting on evidence that no one can produce on demand.

⚠ MONITORING is the status most likely to become a graveyard, so the `/process-backlog` survey lists
these every session alongside the ready counts. If one has been waiting for something that has
since happened, it is no longer monitoring.

⚠ `READY` does not mean approved. Approval happens only when an item is presented in the
`/process-backlog` seat, and the only record of the user's judgement is `PARKED` with their rationale.
