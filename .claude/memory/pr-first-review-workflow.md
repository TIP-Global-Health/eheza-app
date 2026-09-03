---
name: pr-first-review-workflow
description: "PR goes up FIRST, then the review; findings are posted as inline PR comments; I present analysis and wait for approval before fixing"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c7019686-e437-4d8b-9f2d-a2a4c507d45d
  modified: 2026-08-10T09:43:28.738Z
---

Development process, **replacing the pre-push gate** (set 2026-07-27, user's words: *"when starting
a development, before asking for a reiew, open a PR - I want to see the code. Then, after I run a
review, I want the review comments to be made on that PR, so I could see them in Github. Then, you
present your analysis, what needs fixing and don't execute before I approve."*):

1. **Develop, commit, push, open the PR** — before any review. The user wants to read the code on
   GitHub. Issue link is the FIRST body line ([[pr-description-issue-link-first-line]]).
2. **Ask the user to run `/code-review <level>`** — announce the level and why. I do not run it.
   ⛔⛔ **MANDATORY, 100% OF CASES, NO EXCEPTIONS — escalated by the user 2026-08-10:** *"I want you
   to make sure that review is part of development cycle. It should be asked for at 100% of the
   cases!"* The ask is part of finishing a PR, exactly like pushing it. It is not conditional on my
   confidence, on CI being green, on the diff being small, or on the change looking obvious.
   **The message that announces a new PR must contain the review command**, in copy-pasteable form:
   `/code-review medium <branch-name>`. If I have written "PR #NNNN is up" without that line, the
   turn is not finished.
   ⛔ **I keep skipping this — three recorded instances, so treat it as a known failure mode of
   mine, not an oversight:**
   - 2026-07-29 (#2005/PR #2018): opened the PR, went into e2e runs, reported "complete" without
     handing back — user: *"Why don't you ask to run code review on new PR anymore?"*
   - 2026-08-09: ended with a vague *"Want a review on this one?"* instead of the command — user:
     *"don't you ask to run a code review anymore?"* An unactionable offer does not count as asking.
   - 2026-08-10 (B-152a/PR #2065): opened the PR, then moved straight on to reporting another PR's
     CI results — user: *"Why didn't you ask to run a review for #2065?"*
   Green tests are **not** the finish line; the review is. Never summarise an issue as done, and
   never move on to another item, before asking.
3. **Post the findings as inline PR comments** so they are visible in GitHub (mechanics below).
   ⛔⛔ **100% rule — corrected FOUR times (2026-08-18, 2026-08-19, 2026-08-20, 2026-08-24).**

   ⚠ **2026-08-24, B-189 / PR #2136 — the failure mode, named, because knowing the rule did not
   prevent it.** The review agent reported its findings *into the transcript*. I read that as a
   message addressed to me: verified all three findings against the code, wrote the analysis, asked
   the user which to fix — and posted nothing to the PR. User: *"code review findings are supposed
   to be posted on PR, inline if possible. I've asked this over and over again, and you keep missing
   this. What is the reason?"* **The trigger to watch for is the review result ARRIVING IN CHAT.**
   That arrival is not delivery; it is the cue to post. Nothing about the finding's content — how
   minor, how pre-existing, how obviously declinable — changes that.

   📌 **Structural fix applied the same day:** this rule now also lives in the repo, in
   `.claude/skills/process-backlog/SKILL.md`'s hard-rules list, which is read at the start of every
   backlog session. It previously covered only *asking for* the review, so the skill I consult right
   before working was silent on posting — and the memory index one-liner, read hours earlier, was
   the only thing carrying it.

   **Posting the findings is the step that ENDS a review.** Do it before reporting anything to the
   user in chat — not after acting, not after deciding, not "once I've fixed them". A review is not
   finished until its findings are on the PR. `ReportFindings` populates the host UI only; it is NOT
   posting, and it does not discharge this.

   **Post the finding AS RAISED, not your answer to it** (user, 2026-08-20 — the specific miss).
   Each comment leads with the reviewer's finding, quoted, under a `**Review finding** — file:line`
   heading. The resolution goes underneath as `**Resolution: fixed / refuted / skipped**`, or as a
   reply on the thread once the work is done. A comment containing only your conclusion destroys the
   record: the reader cannot see what was raised, only that you were satisfied.

   Covers EVERY finding without exception — ones you fix, ones you skip, ones you refute, ones you
   already fixed before the review reported, and non-blocking observations. The PR is the durable
   record; this session's chat is not.

   Mechanics: inline on the line the finding concerns —
   `gh api repos/O/R/pulls/N/comments --input -` with `{body, path, line, side:"RIGHT", commit_id:<head sha>}`.
   Fall back to a PR-level comment (`gh api repos/O/R/issues/N/comments -f body=...`) ONLY when the line
   is not in the diff — which includes lines a later force-push REMOVED. To revise a posted comment:
   PATCH `pulls/comments/<id>` (inline) or `issues/comments/<id>` (PR-level).
4. **Present my analysis** — which findings are real, which are not worth acting on, and why.
5. **STOP. Do not fix anything until the user approves.** This is the part that changed: approval
   now comes *after* the analysis, not before the review. Once approved, run it through: fix,
   re-verify, push, watch CI to completion — but **ask before any Copilot request** (below).

## ⛔ NEVER request a Copilot review without asking first (2026-08-02)

User's words: *"do not request copilot review automatically. Ask me if it's needed."* This
**overrides** the old backlog convention ("GitHub issue + PR, CI must run, Copilot review
requested") recorded in [[improvement-backlog]] — requesting Copilot is no longer part of the
standing regime for a backlog PR, on the first pass or after a fix. Offer it, name what it would
cover, and wait.

**Why it matters — the quota is small and shared.** Copilot code review spends a monthly
premium-request allowance shared with all other Copilot use, and it resets on the 1st. Observed on
this repo: 18 successful reviews between 22 and 28 July, then **four straight quota-blocked
attempts** from 29 July to 30 July (#2016 ×2, #2018, #2021) — roughly three days with no Copilot
review available at all. A block is not an error the user sees; it posts a review whose body reads
*"Copilot was unable to review this pull request because the user who requested the review has
reached their quota limit"*, with zero inline comments. **Check for that body before reporting a
PR as "Copilot clean"** — two PRs in this stack were recorded as reviewed when they had only the
stub.

**Re-requests are what drain it.** 24 review events across 12 PRs in that window; #2013 alone took
4 and #2007 took 3, because each push was followed by another request. One request when the diff
is final beats one per push. There is no API to read the remaining balance — the billing-usage
endpoints need the `user` / `admin:org` scope the local token lacks, so the count comes from the
GitHub Settings → Billing page, or from counting review bodies as above.

## The push gate is retired (2026-07-27)

The `hooks` block was removed from `.claude/settings.local.json`, so `git push` is no longer
blocked. Verified empirically: with a deliberately wrong marker the push went through, and the
change took effect **without restarting the session**. The script is still at
`.claude/hooks/pre-push-review-gate.sh` if it is ever wanted back; `.git/eheza-review-marker` is
deleted and no longer read. Nothing enforces the review now except this file — the review gates
the **merge**, not the push.

## Posting findings to the PR

One review, inline comments on the lines they concern:

```bash
gh api repos/TIP-Global-Health/eheza-app/pulls/<N>/reviews -X POST --input review.json
```

`review.json`: `{"event":"COMMENT","body":"…","comments":[{"path":…,"line":…,"side":"RIGHT","body":…}]}`.
Build it as a file and use `--input` — `-f comments[][path]=…` does not express an array of objects.
Verified on PR #2011.

⛔ **Inline by default — one finding, one anchored comment.** The body is only for a finding whose
line genuinely is not in a diff hunk, and say so when that happens. (User, 2026-07-28, on a review
posted entirely in the body: *"why is it as one comment in the issue, and not pinpointed to the
spots in code?"* — all three findings had been anchorable; I had not checked.)

**Three levels, in order — never jump straight to the body.**

1. **Line anchor.** `line` must fall inside a diff hunk of the PR's new version, or the POST fails
   with `422 Line could not be resolved`. Check first with
   `git diff <base>..<head> -- <file> | grep "^@@"`.
2. **File-level comment** when the line is not in a hunk but the file IS in the diff:
   `gh api repos/O/R/pulls/<N>/comments -X POST -f commit_id=<head sha> -f path=<file>
   -f subject_type=file -f body=…`. It shows against the file in the Files view. (Missed this on
   PR #2016 and fell back to the body; user asked again why comments were not pinpointed.)
3. **Body** only when the file is not in the diff at all — and say so explicitly, naming which
   finding and why. To fix a review already posted in the body,
repost with anchors and replace the old body via
`gh api repos/O/R/pulls/reviews/<id> -X PUT -f body='…'`.

## ⛔ The review cycle must not grow the diff (2026-08-03)

User, after three review passes on one small PR produced 13 findings: *"You can't have ten review
bugs for PR with 5 lines changed. That does not make any sense."* They were right, and the cause was
mine, not the reviewer's.

**What happened (B-005, PR #2030).** The backlog's traced fix was ~2 lines. I judged it inadequate
and designed a better one: new dedup key, extracted notification helper, item-data persistence
helper, attempt counter with its own budget, retry semantics, try/catch. 152 insertions. Each review
round I answered findings by adding more code — the branch grew 24 → 56 → 82 → 75 lines across four
commits, and nearly every finding was about code *that expansion had added*, not about anything
pre-existing. The branch was reset to `develop` and rewritten as 23 lines, which fixed the actual
defect.

**Rules that follow:**

1. **Fix at the size the item deserves.** Check the backlog's traced fix first; if you are about to
   write something much bigger, that is a decision to surface, not to make silently.
2. **A review round is not a licence to add code.** If answering findings is growing the diff, stop
   and ask whether the change has outgrown the problem.
3. **Triage findings; do not auto-fix all of them.** Pre-existing conditions, out-of-scope items and
   things the wording already covers get declined with a reason. On the rewritten 23-line version,
   6 of 7 findings were correctly declined.
4. **Weigh usage before effort.** This feature had produced 3 exports in its lifetime, the newest
   2025-06-23. That is the same fact that got B-005 parked originally, and it should have bounded
   the work from the start.

## ⛔ Still true: DO NOT RUN `high` REVIEWS

*"you can not run review with level high. It burns tokens like crazy. If you believe this is a
must, you'll need to justify."* Ask the user to run `/code-review medium` for everything. To run
`high` I must first state what makes a medium pass insufficient **for this diff** — path alone is
not justification. Cost data (2026-07-22): five `high` runs ≈ 830k / 1.07M / 1.43M / 1.54M / 1.63M
tokens, ≈6.5M total, 13–17 min each. Counterweight, so the tradeoff stays visible: they did find
CONFIRMED defects a medium pass missed.

**Comment/docs-only diffs:** still ASK (the 100% rule above admits no exceptions), but say in the
same breath that the diff is comments/docs only and a review is probably not worth spending — let
the user be the one to skip it. Superseded the old "skip step 2" wording on 2026-08-10.

## Invoking the review

- ✅ **CORRECTED 2026-08-24: `Skill({skill: "code-review", args: "medium <branch>"})` WORKS** and
  launches it as a background fork. The old note said `/code-review` was a built-in carrying
  `disable-model-invocation` that the Skill tool refused — that is no longer true, and it appears in
  the session's available-skills listing. Verified by running it on `children-seen-counts-children`.
  ⛔ **This does NOT loosen the ask-the-user rule.** The user's process is that THEY run the review
  (they said so: *"after I run a review, I want the review comments to be made on that PR"*), and
  `high` is still never to be auto-run. Invoke it yourself only when the user plainly meant to and
  the command did not register — e.g. it arrived as message text because of a leading space.
- `Workflow({name: "code-review", args: "<level> <target>"})` also exists but accepts
  `high` / `xhigh` / `max` only.
- **`args` MUST be a string.** The script does `typeof args === "string" ? args : ""`, so an object
  is silently dropped and the review falls back to the current checkout's branch diff — reviewing
  the wrong thing while looking clean.
- Pause/resume: `TaskStop` the task id, then re-invoke `{scriptPath, resumeFromRunId}` with the
  identical `args`.

**Why two reviewers:** the local pass and Copilot are decorrelated and catch different things. On
PR #2011 the local pass found an ffmpeg partial-output bug; Copilot independently found that the
`unlinkSync` beside it could still abort the whole loop. Related:
[[request-copilot-review-via-api]], [[verify-by-running-not-reasoning]], [[delete-branch-on-merge]].
