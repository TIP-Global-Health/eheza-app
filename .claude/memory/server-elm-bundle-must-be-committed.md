---
name: server-elm-bundle-must-be-committed
description: Commit checklist when modifying server/elm/src — the compiled JS bundle is checked into git and ships to prod
metadata:
  node_type: memory
  type: feedback
  originSessionId: 4ca60a48-cd68-410e-ad0c-33bb27b1dee0
---

When any file under `server/elm/src/` is edited, the compiled bundle at
`server/hedley/modules/custom/hedley_general/js/elm-main.js` (and
`elm.js` if it changed) must be regenerated AND committed in the same PR
as the source change.

Build command (run from `server/elm`):

```
elm make src/Main.elm --output ../hedley/modules/custom/hedley_general/js/elm-main.js
```

`ddev elm:watch` keeps the file fresh on disk while it's running, but
git status only stages it when explicitly added — easy to forget after
finishing a series of edits and committing source-only.

**Why:** the JS bundle is checked into git and deployed to production
servers as-is — Pantheon does **not** run `elm make` server-side. A PR
that ships only the source change leaves prod users on the previous
bundle, so the new behaviour silently does nothing in production.

**How to apply:** before committing a server-elm-source change, run the
`elm make` command above (or rely on the running `ddev elm:watch` to
have already regenerated it), then `git add` the bundle file
explicitly. Regression check: `git diff origin/<base> -- server/hedley/modules/custom/hedley_general/js/elm-main.js`
should be non-empty whenever `server/elm/src/...` was touched on the
branch.
