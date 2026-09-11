---
name: code-duplication-rule
description: "⛔ 100% rule: no duplication — including the SAME EXPRESSION evaluated twice, not only copied blocks (codified in the repo's CLAUDE.md, PR #2091)"
metadata:
  node_type: memory
  type: feedback
---

# Duplication — every shape of it, 100% of the time

**User, 2026-08-16:** *"Why do we run `(chartsMonth currentDate data)` twice within same
function?"* then *"There should be a firm rule of avoiding code duplications. Don't we have it?"*

We did have one. I broke it, and the wording is why it slipped: the rule named **copied code
blocks**, and what I wrote was **the same pure expression evaluated twice** — no copied block, so
nothing in the wording caught my eye. ⚠ **A rule that names only one shape of duplication is easy
to walk past.** Tightened in the repo's own `CLAUDE.md` (PR #2091) to add:

- a repeated expression → compute once in a `let` (or a variable) and use the result;
- **a helper that exists only to be called twice from one function belongs in that function's
  `let`** — that is the exact smell from the miss: I wrote a top-level `chartsMonth` whose only
  reason to exist was two call sites in one view.

## How to apply it

Before finishing any function, re-read it for: the same call with the same arguments appearing
more than once; a top-level helper with a single caller; two branches that differ only in a
constant. Elm makes this cheap — a `let` binding is always available and always free.

📌 Purity is NOT a defence. "It returns the same value both times" is why it is safe to hoist, not
a reason to leave it.

Related: [[meaningful-issue-and-pr-titles]] (same class of 100% rule), [[pr-first-review-workflow]].
