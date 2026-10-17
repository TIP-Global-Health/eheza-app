# Fallback: driving the run through Claude in Chrome

Use this only when the QA driver cannot run. The extension costs one model turn (~9 s) per
tool call and cannot call the e2e helpers, so every click of a known path is paid for. The
workflow in SKILL.md stays the same; this file covers what changes.

## Before anything: is the tab rendering?

Measure animation frames in one `javascript_tool` call — `document.hidden` and screenshots
prove nothing:

```js
const t0=Date.now(); let frames=0;
await new Promise(res=>{const tick=()=>{frames++; if(frames<40 && Date.now()-t0<1500) requestAnimationFrame(tick); else res();};
  requestAnimationFrame(tick); setTimeout(res,1600);});
JSON.stringify({frames, hidden:document.hidden, visibility:document.visibilityState})
```

A healthy tab reads ~40 frames, `visible`, and a hover costs ~20 ms. **0 frames: do not
proceed** — Elm never repaints, so the DOM freezes while the model moves on. Ask the user to
raise the Chrome window once. If still 0, paint the tab red and ask whether they see it:

```js
document.title='### CLAUDE IS DRIVING THIS TAB ###';
const b=document.createElement('div');
b.style.cssText='position:fixed;inset:0;z-index:2147483647;background:#e4002b';
document.documentElement.appendChild(b);
```

No red means the extension drives a tab Chrome never shows. **Reinstalling the extension is
the only fix found**; window arranging, new tabs and groups, a Chrome restart and a reboot
all failed. The same 0 frames on `https://www.google.com` proves it is not the app.

## Pairing

Create the device as `resetDevice` in `client/e2e/helpers/device.ts` does, with a code other
than `99999999` and a title not starting with "E2E" (e2e runs delete those). To bring the
pairing screen back, wipe local state: unregister the service worker, delete the caches
**including `config`**, delete IndexedDB, clear storage, reload. Enter the code with
`form_input`.

## Rules the extension needs

- **Read state with `javascript_tool` (~1 ms), not `find` (~1.2 s) or screenshots.** Batch
  actions in `browser_batch`; keep a `javascript_tool` body under ~10 s or the connection
  drops (call `tabs_context_mcp` and re-read the page before redoing anything).
- **Native `confirm()`/`alert()`, a native `<select>` opened with the mouse, and the
  browser's autofill popup all lock the extension out of the tab.** Never click them: set
  selects and text fields with `form_input` on a fresh ref. Recovery: close the tab and
  reopen its URL — pairing and IndexedDB survive.
- `form_input` fires nothing when the value already matches. Set `''` first, then the value.
- Clicks that report success but change nothing: count events with capture listeners on
  `mousemove`/`mousedown`. Both 0 means input to the tab is dead; reopen it. Seen at the
  health-centre choice right after a fresh pairing.
- Menu cards answer only on `div.image`. Prefer `ref` clicks; for clickable `div`s the a11y
  tree hides, read the element's rect and scale it by `<screenshot width> / innerWidth` in the
  same call — the viewport changes size between and within sessions.
- The viewport is emulated, with no touch and no device emulation; say so in the report.

## Recording

`gif_creator` is the only capture. Inject `../cursor.js` (re-inject after every navigation),
hover before each click, start capturing before the first action, and export as
`qa-<pr>-<scenario>.gif` with `showClickIndicators`, `showActionLabels` and `showDragPaths`
off. It stops at **50 frames** — export and start again on a long scenario. Then:

```bash
mkdir -p /var/www/html/ihangane/client/qa-recordings/<pr>
ffmpeg -y -loglevel error -i ~/Downloads/qa-<pr>-<scenario>.gif \
  -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" \
  -c:v libx264 -crf 23 /var/www/html/ihangane/client/qa-recordings/<pr>/qa-<pr>-<scenario>.mp4 \
  && rm ~/Downloads/qa-<pr>-<scenario>.gif
```

Every frame gets the same length, so **the video is not real time** — never read timing
from it, and say so when handing it over.

## chrome-devtools-mcp

A second fallback: its own headed Chrome on a fresh profile, rendering reliably, with
`navigate_page initScript` to keep a hook installed across reloads. No batching and no
`gif_creator`; record from `take_screenshot filePath=…` frames joined with ffmpeg's concat
demuxer. E-Heza's clickable `div`s are missing from its snapshot — dispatch mouse events at
their rect from `evaluate_script`.
