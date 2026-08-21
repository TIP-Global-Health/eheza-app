---
name: discovery
description: Run a discovery round over the E-Heza codebase — frame the round, fan out module/lens agents, adversarially verify every candidate, and append verified findings to the improvement backlog. Trigger when the user asks to run discovery, start or continue a round, look for new improvement items, or audit an area for findings. NOT for implementing what is already queued — that is the backlog skill.
---

# Discovery seat — E-Heza improvement rounds

This seat finds and verifies; it does not build. Its deliverable is the backlog and the coverage
map, both left in a state a later session can act on without re-deriving anything.

## The one thing to get right

**Do not ask the user to approve findings.** Discovery appends verified candidates freely; the
user's decision happens later, in the backlog seat, when an item is presented. Asking here stalls
the round and produces approvals nobody recorded.

What the user *does* frame is the round itself — which surfaces to sweep, and how deep. Propose
that, then run.

## State to read first, in this order

All of it is in `.claude/backlog/` (see its README for the layout).

| what | where |
|---|---|
| the live cursor — what merged, what is blocked, the environment gotchas | `.claude/backlog/HANDOFF.md` |
| lens registry + module coverage table + **declined classes** | `.claude/backlog/coverage.md` |
| what is already recorded, so you do not re-find it | `.claude/backlog/index.tsv` + `items/<id>.md` |

**The round number is not written in this file.** Take it from the state and add one:
`cut -f7 .claude/backlog/index.tsv | sort -n | tail -1`, cross-checked against `rounds.md`. Same for what is already cleared, what was declined, and where the
dry-stop counter stands — all of it is state, and state lives in those three files.

The declined list is long and it is **binding**. Do not re-pitch a declined class because this
round found a fresh instance of it.

⚠ Before pitching anything, grep the backlog for the **symptom and the function name**, not just
item ids — a finding has been recorded twice under two ids because only the ids were checked.

## Running a round

**Mode is coverage-sweep**, not thematic lenses. Assign each agent a module or area and have it
audit that unit exhaustively, returning both any findings *and* a coverage verdict. **An
audited-clean verdict is a deliverable** — it fills the coverage table and a round that only
clears modules is productive, not dry.

1. **Frame** — pick the units. Prefer `☐` rows in the coverage table, vertical program slices
   (Elm pages + backend module + RESTful + decoders for one program), and the big shared files.
   Put the plan to the user before spending agents on it.
2. **Fan out** — one `backlog-scout` agent per unit, worktree-isolated. Their brief is in the
   agent definition; do not restate the verification standard in each prompt.
3. **Verify yourself** — every candidate an agent returns, end to end, against
   `knowledge/verification-standard.md`. An agent's confidence is not evidence.
4. **Record immediately, per round, not at the end** — a new item is a new
   `.claude/backlog/items/<id>.md` with its `<!-- id: status: tier: round: -->` line, then
   `reindex.py`. — findings appended to the backlog, coverage
   rows and lessons into the coverage map. Sessions get compacted and agents get killed mid-round;
   anything unrecorded is lost.

**Stop rule:** dry-stop after three consecutive rounds with zero new verified findings (a sweep
that clears modules counts as productive). Hard cap at Round 50.

## What makes a finding worth recording

- It is **verified**, not plausible — you traced it, not an agent's summary of it.
- It is **reachable** — the code path runs, the branch is live, the value is really used.
- It is **not** in a declined class, not already in the queue under another name, and not in an
  area the coverage map marks cleared.
- It says where the evidence is, so the backlog seat can re-locate the anchors and re-decide the
  fix. **Write the mechanism and the evidence; the "Fix shape:" line is a first guess and will be
  read as one.**

Severity and tier are judgements about impact on real users and real data — a finding that reduces
the risk of an un-deployed release is worth more than new surface, and the handoff says whether
anything is deployed.
