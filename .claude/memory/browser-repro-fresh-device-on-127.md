---
name: browser-repro-fresh-device-on-127
description: "Chrome's localhost:3000 holds an old paired device from a live-DB copy; for a fresh demo device use origin 127.0.0.1:3000 (add it to the gitignored LocalConfig.elm) and read pages with get_page_text, since screenshots time out"
metadata:
  node_type: memory
  type: project
  originSessionId: 95616326-80bf-4a1d-9bd6-27886b445edf
  modified: 2026-11-08T14:10:26.316Z
---

Reproducing client bugs in the user's Chrome against the local demo DB (2026-11-08, B-563):

- `http://localhost:3000` in the user's Chrome is already paired as "B317 verify device (live db)"
  (Mushongi HC, a live-DB copy). Do not clear that site data.
- A separate origin gets its own IndexedDB and service worker: add `( "127.0.0.1", local )` to the
  gitignored `client/src/elm/LocalConfig.elm` (gulp rebuilds in seconds), open
  `http://127.0.0.1:3000`, pair with demo code `12345678`, PIN `1234`, Nyange Health Center, then
  Device Status → "Start syncing" for the HC. Revert the LocalConfig line afterwards.
- The ClinicsPage "Not Found" race needs an F5 ([[clinics-not-found-race]]).
- `debug = True` in LocalConfig makes the dev bundle so heavy that `computer` screenshots often
  time out (CDP 30 s); `get_page_text` answers reliably, and clicks still land — read the page by
  text, screenshot only for coordinates. The 10 s `wait` cap means long waits go to a
  background `sleep`.
- Demo participants for the Nyange I FBF group: E2ETest B548MomA/B/C with one child each.

**Why:** the shared browser state is the user's, and the demo DB knows nothing of the live-copy
device, so a second origin is the only non-destructive fresh device.
**How to apply:** any browser reproduction or QA against the local demo DB that needs a clean
device; see also [[verify-sync-lanes-in-browser]] for the sync-specific recipe.
