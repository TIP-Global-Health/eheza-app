---
name: verify-tool-capabilities
description: Before saying a tool or API cannot do something, check its current docs and the installed version's --help.
metadata:
  node_type: memory
  type: feedback
  originSessionId: 1feaa95e-7dbb-4995-a675-77b648ab10fc
  modified: 2026-10-13T07:30:40.242Z
---

Before claiming a tool, CLI or API cannot do something, check its current docs or changelog
and the installed version's `--help`, and name the version when it matters.

**Why:** I told the user GitHub had no way to upload videos to comments and built a Chrome
workaround. The CTO pointed out `gh pr comment --attach` (gh 2.99, 2026-09-01). The machine
also had gh 2.45 from Ubuntu's repo, too old to have it.

**State (2026-10-13):** gh 2.102 installed to `~/.local/bin/gh` (first on PATH) from the
verified release tarball; Ubuntu's 2.45 remains in `/usr/bin`. `gh issue create --attach` works too.

**How to apply:** treat "X is impossible" from memory as a guess. Check docs plus local
`--version`/`--help` first, and if the local tool is outdated, say so and propose the upgrade.
