---
name: process-backlog
description: Work the E-Heza improvement backlog with the user — survey what is queued and in which tier, present an item for their decision, then build and ship the ones they approve. Trigger when the user wants to work the backlog, asks what items are queued or what the tiers hold, asks to see or present an item, or names an item to implement (e.g. "what do we have?", "what is in tier 2?", "present the scoreboard one"). NOT for finding new items — that is the run-backlog-discovery skill.
---

# Backlog seat — E-Heza improvement work

This is the seat where queued findings become work. It is a loop over the queue, not a job on
one item: the user opens with "what do we have", picks something to see, decides, and only then
is anything built.

## The one thing to get right

**Items in the backlog are verified candidates, not approved work.** Discovery appends without
asking anyone. Nothing in the queue has been agreed to, no matter how long it has sat there or
how high its tier. `READY` means *verified and not yet put to the user* — it never means
"go ahead".

Approval exists in exactly one place: step 3 below. The only durable record of the user's
judgement is `🅿 PARKED` with their rationale, quoted verbatim. **A parked item is never
re-pitched** — not in a later session, not as part of a batch, not "briefly, since it came up".

## State to read first

The queue and its history are files, not memory of this conversation. Read them at the start of
every session; they move under long ones, so re-read a tier line before picking from it.

All of it is in `.claude/backlog/` (see its README for the layout).

⛔ **Check the main tree's branch before reading any of it.** The files come from whatever branch
`/var/www/html/ihangane` is checked out on, and bookkeeping is only ever committed on `develop` — so
a tree parked on a feature branch serves a stale queue silently. Run
`git log --oneline HEAD..origin/develop -- .claude/backlog/` first; if it is non-empty, read from
`origin/develop` and do the bookkeeping in a `develop` worktree instead of switching the main tree.

| what | where |
|---|---|
| the index — one row per item, derived | `.claude/backlog/index.tsv` |
| the full entry for one finding | `.claude/backlog/items/<id>.md` |
| priority-queue header: tier lines, conventions, gotchas | `.claude/backlog/queue.md` |
| the live cursor: what is open, blocked, in flight | `.claude/backlog/HANDOFF.md` |
| verification lessons, cleared areas, declined classes | `.claude/backlog/coverage.md` |

Where the handoff and an entry disagree, **the handoff wins** — it is rewritten against GitHub
and live databases, the entry is a snapshot from the round that found it.

## The loop

### 1. Survey
Answer "what do we have, and in what tier?" from the index — one command, not a file read:

```bash
awk -F'\t' '$3=="READY"' .claude/backlog/index.tsv | cut -f2 | sort | uniq -c   # ready, by tier
awk -F'\t' '$3=="MONITORING"' .claude/backlog/index.tsv                          # waiting on evidence
awk -F'\t' '$3=="PARKED"' .claude/backlog/index.tsv                              # never re-pitch
```

Then say what is READY per tier, what is gated on a decision or a live check, and what is parked.
`UNKNOWN` means the entry states no lifecycle status — treat it as not-ready and read the entry.

**Always name the MONITORING items and what each is waiting for.** They have no action today,
which is exactly why they go unmentioned and rot. Check whether the thing being waited on has
happened — a deploy that shipped, a window that has passed — because when it has, the item is
not monitoring any more and nobody else will notice. Give counts, not a recital
of 200 entries. Say plainly if a tier is empty — that is a real answer and it has ended rounds
before.

### 2. Present
The user names one. Lay out, from the entry **and from the code as it stands today**:

- the mechanism — what actually goes wrong, traced
- the impact — who sees it, and whether it is live (see the release caveat in the handoff)
- the fix you would make, and why that one
- the cost — files touched, tests needed, CI exposure
- anything the entry flags as gated: a clinician call, a live query, a sibling item it interacts with

Re-locate the anchors first. Line numbers drift; snippets and function names are authoritative.
And if anything about the *intended* behaviour is open, read the issue behind the code before
presenting — see the hard rule below.

### 3. Wait for the decision
Stop. The answer is "implement", "park", or "skip". Do not start building because the item looks
obvious or small.

On **park**: write `🅿 PARKED` into the entry *and* the tier line with the user's words verbatim
and the date, before moving on.

### 4. Build
Follow `knowledge/runbook.md` exactly — worktree per finding, verification gates per change type,
issue → PR → review request. It is validated across dozens of PRs and its warnings were each paid
for once already.

### 5. Record
Immediately, before the next item and before anything can be compacted away: entry status, PR
number, tier line, and any side-findings noticed while building appended as new minors. Then
`.claude/backlog/reindex.py` — the index is derived and must not be edited by hand.

## Hard rules

- ⛔ **Never switch or modify the main tree** (`/var/www/html/ihangane`) — the user works there in
  a parallel terminal. One isolated worktree per finding, released the moment the PR is up.
- ⛔ **Ask for the review, every time.** The message announcing a PR must carry the copy-pasteable
  `/code-review medium <branch>`. A PR announcement without it is an unfinished turn, and a vague
  "want a review?" does not count. This is a known, repeated failure — see
  `pr-first-review-workflow` in memory.
- ⛔ **Post every review finding to the PR, inline, BEFORE reporting anything about the review in
  chat.** The review agent reports back into the transcript — **that is not delivery.** A review is
  not finished until its findings are on the PR, anchored to the lines they concern, one comment per
  finding, phrased as the finding was RAISED rather than as your answer to it. This covers findings
  you fix, skip, refute, or had already fixed. Only when a line is genuinely outside a diff hunk do
  you fall back to a file-level comment, and only then to the PR body — saying which and why.
  ⛔ This is a **known, repeated failure of mine — four recorded instances** (2026-08-18, 08-19,
  08-20, 08-24). The 08-24 one went: review agent replied in chat → I verified the findings and
  wrote the analysis for the user → posted nothing. Mechanics and the full history are in
  `pr-first-review-workflow` in memory.
- ⛔ **Close the loop on every review thread when the fix lands**, and **commit + push the backlog
  files on `develop`** in the same step that records them. Three separate misses in one day traced
  to the same shape — work lands, record lags — so treat "the record disagrees with the repository"
  as a defect in its own right. A `Stop` hook commits the files as a backstop; it does not excuse
  skipping the step.
- ⛔ **When the requirement is in doubt, go and read it.** Any question about what the code is
  *supposed* to do — is this verdict the right way round, is this threshold correct, was this
  deliberate — is answered by the issue, not by reasoning from the code. The trail:
  `gh api repos/TIP-Global-Health/eheza-app/commits/<sha>/pulls` → the PR → its body's issue link
  → `gh issue view <n>`. Feature work here carries a written spec in the issue, often step by step;
  ⛔ do not present a fix direction, and do not build one, while that spec is unread.
  ⚠ Read it before deciding the fix is a revert, too: on B-235 the spec's exact wording
  (*"equal to or greater than"*) made the pre-flip code wrong at the boundary as well, so reverting
  the offending commit would have shipped a second, quieter defect.
- ⛔ **Never request Copilot without asking first.** Offer it, say what it would cover, wait.
- CI must run on these PRs — no `[ci skip]`. **The user merges**, with `--delete-branch`.
- Do not generate new proposals here and do not re-mine cleared areas. That is the discovery
  seat's job, and duplicating it wastes both.
- Titles and bodies follow the repo's `CLAUDE.md` rule: the title names the defect from the
  reader's side and says where; the body describes current state only — no verification section,
  no command output, no review history.

## The entry describes the defect; the fix is yours to choose

Every entry ends with a "Fix shape:" line. **It is the finder's first guess, written while
tracing the mechanism — not a specification.** Items have been built to that line and been wrong,
each corrected only by reading the surrounding code: a helper was duplicated that the file already
called three times; a defect was framed backwards and two fixes were built on the bad framing; an
entry named one call site when there were five.

Read the entry for the mechanism and the evidence, then decide the fix from the code in front of
you. Before writing a helper, grep for whether one exists and whether the file already imports it.
Before narrowing to what the entry lists, grep for the pattern it describes.

And verify at **every layer the value travels through**, not just the first — a payload that
satisfies the Elm decoder can still fatal the PHP that receives it.
