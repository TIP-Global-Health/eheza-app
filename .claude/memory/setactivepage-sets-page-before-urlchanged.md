---
name: setactivepage-sets-page-before-urlchanged
description: "⛔ in App/Update.elm, SetActivePage writes model.activePage BEFORE pushUrl fires UrlChanged — so `activePage /= model.activePage` inside UrlChanged is False for every in-app navigation"
metadata:
  node_type: memory
  type: project
  originSessionId: 5405ad61-b5be-49a7-ab36-d007a7a92e67
  modified: 2026-10-15T08:25:13.149Z
---

`SetActivePage page` returns `{ model | activePage = page }` and `Nav.pushUrl`; `UrlChanged` runs afterwards and finds `model.activePage` already equal to the new page. A "did the page change?" test inside `UrlChanged` therefore only fires for reload-free back/forward and typed URLs — never for taps.

**Why:** PR #2359 (B-390, 2026-10-15) — round-1 review fix added that comparison to gate the GPS clear/request; it silently disabled both on normal navigation, making the PR worse than develop. Round 2 caught it. I had read `SetActivePage` only as far as its `onEntry` guard.

**How to apply:** for per-entry work, either use `SetActivePage`'s `onEntry` (taps only) or act unconditionally in `UrlChanged` (all paths). Read a handler to its return tuple before reasoning about its ordering. See [[verify-by-running-not-reasoning]].
