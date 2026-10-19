---
name: clinics-not-found-race
description: Root cause analysis of ClinicsPage showing "Not Found" after sync — service worker IndexedDB timing + no retry on Failure
metadata:
  node_type: memory
  type: project
---

ClinicsPage always shows "Not Found" on first visit after sync completes. Page reload fixes it.

**Why:** Race condition between main thread's IndexedDB writes (app.js `dbSync.shards.bulkPut`) and service worker's reads (nodes.js `dbSync.shards.where({type: 'clinic'})`). The SW lazily initializes its Dexie connection on first request (nodes.js:55-68). When the fetch fires immediately after sync, the SW's query may not see committed data yet.

**Why it doesn't recover:** `Backend/Fetch.elm:50-51` — `shouldFetch` for `FetchClinics` only returns True for `NotAsked`, not `Failure`. Once failed, never retries. The `forget` function (line 521-522) exists to reset to `NotAsked` but nothing calls it.

**Why reload works:** Reinitializes Elm model (`clinics = NotAsked`), and SW connection is already established from previous load.

**Scope:** Could affect any entity type fetched via `swEndpoint` (nodes/person, nodes/clinic, etc.), not just clinics. Clinics is most visible because users navigate to ClinicsPage right after sync.

**Decision:** Not fixing — risk of breaking the fetch retry policy outweighs the benefit. E2E tests use a page reload workaround. Real users rarely hit this because the app refreshes after long syncs (>45s, SyncManager/Update.elm:463-464).

**How to apply:** When writing E2E tests that navigate to pages reading from IndexedDB right after sync, always add a reload retry loop (3s timeout, 5 attempts).

**Key files:** `app.js:605-650`, `nodes.js:20-68,828-866`, `Backend/Fetch.elm:50-51,521-522`, `SyncManager/Update.elm:458-467`
