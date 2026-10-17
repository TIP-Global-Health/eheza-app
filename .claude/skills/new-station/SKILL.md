---
name: new-station
description: Set up E-Heza work on a new machine (a "station"), or check what a station is missing. Use when moving work between machines, or when memory, hooks or local settings seem absent.
---

# Moving a station

Work moves between machines through the repository, so anything the work depends on belongs in it.

These travel because they are tracked:

- `.claude/backlog/` — the queue and its entries.
- `.claude/memory/` — the accumulated rules, verification lessons and declined-work decisions.
- `.claude/skills/`, `.claude/agents/`, `.claude/scripts/`.
- `.claude/settings.json` — wires the `Stop` hook that commits and pushes the backlog on `develop`,
  so records cannot be left behind on the station that made them.

These do **not** travel, and each new station recreates them:

| what | why it stays | cost of not having it |
|---|---|---|
| `.claude/settings.local.json` | globally gitignored | more permission prompts; nothing breaks |
| `.git/info/exclude` | per-clone by design | `.claude/hooks/` is excluded here only |
| `client/src/elm/LocalConfig.elm`, `.ddev/config.local.yaml` | gitignored config | covered by Local Setup in `CLAUDE.md` |
| `server/.pantheon-*/` | deploy checkouts | recreated by the deploy skills |
| `<repo>-wt/` | per-item worktrees | recreated by `.claude/scripts/new-worktree.sh <branch>` |

## Memory

Memory lives in the repo only because each station points Claude Code at it. Set
`"autoMemoryDirectory": "<repo>/.claude/memory"` in `.claude/settings.local.json`.

That key is deliberately ignored in checked-in project settings, so it has to be set per station.

Without it a station writes to `~/.claude/projects/<project>/memory/` and the two copies drift.
