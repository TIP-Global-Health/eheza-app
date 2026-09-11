---
name: rollbar-postdeploy-triage-2026-07
description: "Rollbar triage after the 2026-07-07 live deploy (Rwanda 661430 / Burundi-vhw 662688) — obstetric_history_step2 back-compat break jams old devices' sync; reg.update() no-catch is the new top noise; str.trim crash site located"
metadata: 
  node_type: memory
  type: project
  originSessionId: 351b577b-f3e2-4de9-a837-25b5652379b6
  modified: 2026-07-23T18:30:56.582Z
---

Triage of both Rollbar projects on 2026-07-23, covering items since the 2026-07-07 live deploy of PR #1752 (Rollbar fixes) + #1765 (May-11+ developments). Projects: Site-Rwanda = ihangane (prj 661430), Site-Burundi = vhw (prj 662688).

**All five PR #1752 fixes verified working** on updated devices: NetworkError/Timeout suppressed (Rwanda #3: 24k lifetime → 87/30d tail), Dropzone `files` (Rwanda #9 / Burundi #7) last seen pre-deploy, geolocation noise (Burundi #2/#3, 86 occ each) stopped dead on Jul 7, translate/childNodes (Burundi #4) pre-deploy only.

**KEY REGRESSION (action needed): obstetric_history_step2 decode break on non-updated devices.**
Chain: f620da93c (in #1765, live Jul 7) dropped `field_obstetric_history` from the REST handler `$multiFields` → API no longer outputs `obstetric_history`; pre-Jul-7 clients had `required "obstetric_history"` in decodeObstetricHistoryStep2 (removed by 1dac72459) → any old-app device downloading ANY obstetric_history_step2 record gets Http.BadPayload → download batch retries forever = **download lane jammed until the app is updated**. Rollbar items Rwanda #53/#65/#112/#115 (grouped by batch index; #53's 2-week count 17 vs 27/2mo = post-deploy spike). Affected devices seen Jul 9–21: Kagogo Pascasie, Akigabiro Esperance, Akigabiro UWIRINGIYE, Nkona Niwemutoni, Nyampamo Jean Nepomuscene, Kityazo Charles, Kabuye Martine, Kabuye Xavera, Rukoma CHW 1. Remedies: (a) update the app on those devices (SW update path still works); (b) optional server shim = re-add `field_obstetric_history` to the handler's $multiFields (old clients decode again — empty/'none' values fine; new clients ignore unknown fields). Burundi shows NO occurrences of this class.

**GitHub issues opened 2026-07-23 for Opus 4.8 to implement:** #1988 (catch `reg.update()` rejection), #1989 (investigate str.trim in PatientsSearchForm), #1990 (guard bulkPhotoFetch port for missing bulkPhotos.js). Full implementation detail lives in the issue bodies.

**New real issue #2 (small fix): unhandled `reg.update()` rejection.** app.js `case 'Update':` `getRegistration().then(reg => reg.update())` has no `.catch` → offline update-check = uncaught "TypeError: Failed to fetch" (no stack). Rwanda #113 (24 occ/23 IPs, from Jul 9) + #116, Burundi #27. Clusters on #device/#pincode pages (manual "check for update" while offline). Fix: catch+ignore; also guard `reg` undefined. This is now the top JS-side noise, same class 1752 killed on the Elm side.

**Pre-existing active bug: `str.trim is not a function`** (Rwanda #91: 122 occ/102 IPs over 1yr, ~1 per device, latest Jul 21 on CURRENT bundle — verified by matching stack offsets to prod Main.js; Burundi #9). Crash site: Components/PatientsSearchForm/Update.elm SetSearch → String.trim, reached via debouncer emit; msg payload non-string, which pure Elm can't produce — suspect env/DOM-level corruption; needs its own investigation or a defensive port-side look.

**Watch-only (1-off, post-deploy):** Rwanda #114 `self.bulkPhotos` undefined (reading 'handleBulkPhotoFetch', Jul 9 — index.html/app.js version skew during update; cheap guard possible), Rwanda #117 TypeError reading '$' inside Pages/Nutrition/Activity/Update generateNextStepsMsgs (Jul 16, nutrition-photo page — nextTask undefined, theoretically impossible), Burundi #11 person field stored as raw Elm `{"$":"Just","a":{"$":"EntityUuid",...}}` (encoder leak, 5 occ/10mo), Rwanda #24 UUID-already-used conflict, #111 Access denied (deploy-window transient), DB-connection/HTML-error-page/SW-timeout infra items.

**Fleet update status (as of Jul 23):** Burundi/vhw quiet since Jul 13 — looks fully updated. Rwanda still had old-app devices erroring Jul 21 (the jammed list above + NetworkError tail last seen 3d ago).

Related: [[improvement-backlog]], [[design-brief-sync-jam-visibility]] (this jam is a live example of the poison-batch visibility problem).
