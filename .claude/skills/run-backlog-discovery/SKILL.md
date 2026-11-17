---
name: run-backlog-discovery
description: Run a discovery round over the E-Heza codebase — frame the round, fan out module/lens agents, adversarially verify every candidate, and append verified findings to the improvement backlog. Trigger when the user asks to run discovery, start or continue a round, look for new improvement items, or audit an area for findings. NOT for implementing what is already queued — that is the process-backlog skill.
---

# Discovery seat — E-Heza improvement rounds

This seat finds and verifies; it does not build. Its deliverable is the backlog and the coverage
map, both left in a state a later session can act on without re-deriving anything.

## The one thing to get right

**Do not ask the user to approve findings.** Discovery appends verified candidates freely; the
user's decision happens later, in the backlog seat, when an item is presented. Asking here stalls
the round and produces approvals nobody recorded.

What the user *does* frame is the round itself — which surfaces to sweep, and how deep. Propose
that, **wait for an explicit go, and only then run.** Reading state and writing the plan is free;
spawning the first agent is not — nothing fans out until the user has approved the plan.

## Night mode — an unattended round

`.claude/scripts/night-discovery.sh` starts a round at night and says so in its prompt. Nobody is
there: **never ask a question, never wait for approval.** The plan gate above does not apply.

Everything else holds: verify every candidate yourself, respect the declined list, read no open PR,
record as you go.

**Units — two scouts per round, chosen in this order.** Five parallel scouts use up a whole
five-hour session window; the launcher runs several small rounds instead.


1. **Code PRs merged to `develop` since the previous round's commit** (in `rounds.md`), read as
   merged. **At most two scouts**; group the PRs by area. No merges, no such unit. PRs a stopped
   round still owes come first.
2. **The oldest `R<n>` rows in the coverage table's "Last swept" column** fill any
   scout left over. Skip `↑` and `—` rows.

For each re-swept area add a new coverage row with this round, and change the older row of that
area to `↑ R<n>`.

**The round cap does not apply at night.** The launcher keeps the dry-stop: it pauses itself after
three rounds that add no item.

**Wait for every scout before the final answer.** The session ends with that answer, and a scout
still running then is lost.

**If something blocks the round** — a scout that cannot start, a tree not on `develop` — record
what happened in the handoff entry and end. Do not improvise around it.

**Handoff entry.** Before ending, add one short entry at the top of `## Night discovery runs` in
`HANDOFF.md` (create it above `## Open right now` if missing). Re-read the file immediately before
writing.

Keep it to a few lines: the date and round number, the usage figures from the launch prompt, the
units, the new items with tier, and anything that failed. The user reads it to be briefed.

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
   Present the plan to the user: round number, the units (one line each — what it covers and
   why it was picked), the agent count, and what is deliberately left out. **Then stop and wait
   for approval.** This is a blocking gate, not a courtesy — do not spawn any agent, and do not
   write to the backlog, until the user says go. If they adjust the plan, revise and re-present.
2. **Fan out** (after the go) — one `backlog-scout` agent per unit, worktree-isolated. Their brief is in the
   agent definition; do not restate the verification standard in each prompt.
3. **Verify yourself** — every candidate an agent returns, end to end, against
   `knowledge/verification-standard.md`. An agent's confidence is not evidence.
4. **Record immediately, per round, not at the end** — each finding is a new
   `.claude/backlog/items/<id>.md` with its `<!-- id: status: tier: round: -->` line, followed by
   `reindex.py`; coverage rows and lessons go into the coverage map. Sessions get compacted and
   agents get killed mid-round; anything unrecorded is lost.

**Stop rule:** dry-stop after three consecutive rounds with zero new verified findings (a sweep
that clears modules counts as productive). Hard cap at Round 50, unless the user raises it for a round.

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
