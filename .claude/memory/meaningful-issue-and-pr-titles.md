---
name: meaningful-issue-and-pr-titles
description: "⛔ MUST, 100% of the time: titles name the DEFECT not the mechanism, AND issue/PR bodies describe current state only — no verification sections, no process narrative (now codified in the repo's own CLAUDE.md)"
metadata:
  node_type: memory
  type: feedback
  originSessionId: 5739f28b-8e7a-4b1c-bb51-d360c41af95c
  modified: 2026-08-11T09:40:49.372Z
---

# Titles, and the text around them — get these right every time

**User, 2026-08-11 (verbatim):** *"Having meaningful titles for issues and PRs is absolutely must.
Make this crystal clear at project memory, and make sure to follow this 100% of the times."*

Said after catching **two bad titles in one day**, both mine, both merged-log-facing.

## The rule

**A title starts with the FEATURE AREA, then states what was WRONG, from the reader's side.** Not
the mechanism I picked to fix it, and not the activity I performed.

    <Feature area>: <what was wrong / what the change makes true>

**User, 2026-08-16 (verbatim):** *"You're still not titling the issues with enough info. #2092 for
example. Title should start with the feture we're referring to. In this case Dashboard stats."*

⚠ Said after I had already been applying the defect-not-mechanism half correctly — *"Stop a second
measurement at one encounter from breaking the statistics"* names the defect but never says WHERE,
so a reader scanning the log cannot tell which part of the product is affected. **Naming the defect
is necessary and not sufficient.**

Area names in use (match the product/codebase vocabulary, not the file path): `Dashboard stats`,
`Child scoreboard`, `Patient search`, `Wellbeing`, `Sync`, `CI`, `Conventions`. Retitled in one
pass 2026-08-16: #2080/#2081/#2082, #2078/#2079, #2085/#2086, #2087/#2088, #2089/#2090,
#2092/#2093, #2091.

The test to apply before typing it: *someone scanning the merge log or a release note, who was not
in this conversation, learns what was broken.* If the title only tells them what the code now does,
it is wrong — it reads as a preference change, and the fix looks optional.

Secondary rules:
- **The PR inherits its issue's framing.** If the issue title is right and the PR title is not,
  that mismatch is the tell. (#2060's issue title was good; its PR's was not.)
- **Match the register of the surrounding history.** This repo's good titles are plain sentences
  starting with a verb: *Stop…*, *Let…*, *Make…* — describing a behaviour that changes.
- **A cluster PR is titled by its headline defect**, not by the list of parts.
- ⛔ **Never title by the solution** ("use v4", "add pipefail", "refactor X") when a defect exists.

## The two misses that produced the rule (keep — they are the calibration)

**#2061.** Titled *"Give client-created nodes a random UUID"* — the mechanism. It fixes silent
record loss that had **already happened twice in production**, and the title conveyed none of that;
it read like a preference. Its own issue #2060 had it right: *"Client can mint the same UUID for
two nodes created microseconds apart."* Retitled to **"Stop two nodes created microseconds apart
from sharing a UUID"**. ⚠ **Why it slipped: it was the one fix where the SOLUTION was more
interesting than the defect** (v5→v4, with a measurement showing v5 was also slower) — so I titled
it after what I found interesting rather than what the reader needs.

**#2067 / #2066.** Titled *"Make the simpletest job able to fail"* / *"The only server-side test job
passes when no tests run"*. The PR title is ambiguous to the point of being misleading — it reads as
if the change *introduces* failures, which is the opposite of the intent. Retitled to
**"Stop CI passing when the tests never ran"** and **"CI reports success when no tests ran"**.

## The register that works (2026-08-09/11, all from this repo)

| ✅ | ❌ |
|---|---|
| Stop a person edit from erasing recorded GPS coordinates | Give client-created nodes a random UUID |
| Stop counting an absence as a session attended | Make the simpletest job able to fail |
| Stop CI passing when the tests never ran | |
| Let the scoreboard report cancel, and refer on diarrhea | |
| Make gulp publish fail when the build fails | |
| Let a failed deploy say so | |

## ⛔ Bodies too: current state only, NO verification section (user, 2026-08-11)

The title rule is half of a larger one the user then codified **in the repo's own `CLAUDE.md`**
(PR #2068, sections `Code Comments` + `Issues and Pull Requests`), adopted from a sibling Gizra
project. ⚠ That source repo is PRIVATE and eheza-app is PUBLIC — adopt the wording, never cite the
source in a commit, PR, issue, or the file.

**The single idea: describe what is true NOW, not how it came to be.** It governs comments and
issue/PR text alike:

- **NO verification sections.** Do not list how the change was tested, which commands were run, or
  what they printed. CI reports that; in the permanent record it is noise. ⚠ **This reverses what I
  had been doing** — every PR on 2026-08-09/11 (#2055, #2057, #2059, #2061, #2063, #2065, #2067)
  carried a "Verified by running" table. Keep running things before claiming them; put the evidence
  in the CONVERSATION, not the PR body.
- **No process narrative** — no review rounds, no "the first attempt did X", no account of what was
  tried and abandoned. (#2065's body had a "Why not exclude attendance altogether" section; #2057's
  had "Review history".)
- **Comments: current behavior only** — no change history, no issue numbers, no "used to work like
  X". ⚠ Also something I did repeatedly: the `hedley_activity` join comment read as a changelog
  entry, and a docblock literally apologised for the function's name instead of the name being
  fixed.
- An issue says what is wrong and how to see it; a PR says what the change makes true.
- Scope notes ("deliberately not in this PR") are FINE — they describe the change's boundary, which
  is current state, not history.

## Mechanics

- Retitle with `gh api repos/TIP-Global-Health/eheza-app/{pulls,issues}/<N> -X PATCH -f title="…"`.
  ⛔ `gh pr edit` is broken on this repo — see [[gh-pr-edit-projectcards-workaround]].
- Retitling is cheap and non-destructive; **fix a bad title as soon as it is noticed**, on an open
  PR or a filed issue.
- The **commit message** is a separate artifact: leave a pushed commit alone rather than
  force-pushing to fix a headline, provided its body is already problem-first.

Related: [[pr-description-issue-link-first-line]] (the body's first line must link the issue),
[[pr-first-review-workflow]] (the review ask, also a 100%-of-the-time rule).
