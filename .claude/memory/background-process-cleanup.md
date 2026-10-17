---
name: background-process-cleanup
description: Clean up my own background processes (elm make, elm-review, dev servers) when no longer needed — don't leave them running
metadata:
  node_type: memory
  type: feedback
  originSessionId: 4f5b559d-fbf7-4113-b0d6-7d1f42479e0e
---

When I start background processes — `elm make`, `elm-review`, dev servers, long `ddev` commands — clean them up once they are no longer needed rather than leaving them running.

**Why:** The user said "kill shell. do not leave such processes running when not needed anymore." Stray processes consume resources and clutter the machine.

**How to apply:** After a background command has served its purpose, verify it actually exited and kill any leftover/orphaned child processes. Distinguish *my* processes (clean those up) from the *user's* deliberate builds/watchers and the harness-managed MCP servers (`chrome-devtools-mcp`, `playwright-mcp`) — never touch those. See [[dont-kill-gulp-publish]].
