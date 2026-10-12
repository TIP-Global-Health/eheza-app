---
name: dont-kill-gulp-publish
description: '"kill shell" never authorizes killing the user''s `ddev gulp publish` build — leave it running'
metadata:
  node_type: memory
  type: feedback
  originSessionId: 4f5b559d-fbf7-4113-b0d6-7d1f42479e0e
---

Do not kill `ddev gulp publish` (or its `elm make` child) — the user runs it deliberately as a build step.

**Why:** When the user said "kill shell", I treated the only running process — their `ddev gulp publish` build — as the target and killed it. That was wrong; the user corrected with "Don't kill gulp publish." (Compounding error: my first kill attempt used `pkill -f "gulp publish"`, which also matched its own shell command line.)

**How to apply:** "kill shell" refers to a stuck or stray shell, not a deliberate long-running build or watcher. Before killing any process I did not start, identify it specifically. If it is `ddev gulp publish`, `ddev gulp`, `ddev elm:watch`, or a similar user-initiated build/watcher, leave it alone and ask which process they mean. Never use `pkill -f <pattern>` where the pattern also matches the kill command itself — kill by explicit PID instead. See also [[dev-compile-watch-loops]].
