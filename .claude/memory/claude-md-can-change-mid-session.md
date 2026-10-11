---
name: claude-md-can-change-mid-session
description: The CLAUDE.md copy in context is a snapshot from session start; a PR merging to develop can add rules the session never sees
metadata:
  type: feedback
---

The project CLAUDE.md loaded into context is a **snapshot taken when the session started**. When
another session merges a convention change to `develop` mid-session, the copy in context stays
stale — the worktree on disk has the new rule, the context does not, and nothing announces the
difference.

**Why:** on 2026-09-16 the session snapshot was `de5697b94` (10:12); PR #2247 added
*"Keep comments, and paragraphs in `.md` files, to 30 words; CI rejects longer ones a PR adds,
except in tests"* to CLAUDE.md at 10:58, with `ci-scripts/check_comments.py` enforcing it in CI.
The B-345 branch was cut from that very merge commit, and a 100-word doc comment went in anyway
and turned CI red — the context copy still said only "Keep comments short".

**How to apply:** when a worktree is cut from an `origin/develop` that has moved since the session
started, `git diff <session-start-sha>..origin/develop -- CLAUDE.md` (and glance at new
`ci-scripts/`) before writing code. Cheap, and it catches exactly the rules CI will enforce.
Related: [[local-verification-vs-ci]], [[elm-review-before-every-push]].
