---
name: deploy-release
description: Guide and perform an E-Heza deploy to Pantheon for ONE site, then optionally promote Dev→Test→Live and cut the GitHub release + changelog. Trigger when the user wants to deploy, release, ship, push to Pantheon, promote to Test/Live, go live, or generate release notes for a site (rwanda / burundi / somalia). Drives the safe/preparatory steps itself and pauses for the interactive or production-affecting commands.
---

# Deploy & Release (E-Heza → Pantheon)

This skill walks one **single site** through deploy → (optionally) Test/Live promotion → GitHub release. It mirrors the team runbook at https://github.com/Gizra/ihangane/wiki/Deployment and the commands in `server/RoboFile.php`.

## Execution model — who runs what

You (Claude) **drive the safe steps** and **pause at the risky ones**. Never run a step marked 🔴 yourself.

- 🟢 **You run** (via Bash on the host): git prep on `main`, all read-only preflight checks, tag math, `generate:release-notes`, and the GitHub release (`gh`).
- 🔴 **User runs** (tell them the exact command; they run it with `! <command>` so output lands here, then you continue): anything interactive or production-affecting — `ddev auth ssh`, `ddev gulp publish`, `ddev robo deploy:pantheon`, `ddev robo deploy:pantheon-sync`, and post-deploy `fra`.

Pausing matters because `ddev robo deploy:pantheon` shows a `git status` of the Pantheon repo and an interactive **"Commit changes and deploy?"** prompt — a human must review that change-set before confirming. `ddev auth ssh` is also interactive.

**One site per run.** To deploy several sites, finish this flow, then re-invoke the skill for the next site.

## Site → config mapping (memorize before anything else)

| Target          | `EHEZA_SITE` | `PANTHEON_NAME` | Pantheon clone dir (under `server/`) |
| --------------- | ------------ | --------------- | ------------------------------------ |
| Rwanda          | `rwanda`     | `ihangane`      | `.pantheon-ihangane`                 |
| Burundi (vhw)   | `burundi`    | `vhw`           | `.pantheon-vhw`                      |
| Burundi (uvl)   | `burundi`    | `uvl`           | `.pantheon-uvl`                      |
| Somalia         | `somalia`    | `tip-somalia`   | `.pantheon-tip-somalia`             |

⚠️ Two distinct Burundi sites share `EHEZA_SITE=burundi` but have different `PANTHEON_NAME`. Always pin the exact Pantheon site with the user — never assume "burundi".

---

## Step 0 — Gather inputs (ask the user)

Use **AskUserQuestion** to collect:

1. **Target site** — Rwanda / Burundi-vhw / Burundi-uvl / Somalia. Resolve to `EHEZA_SITE` + `PANTHEON_NAME` from the table.
2. **What to do** —
   - *Deploy to Dev* (push Pantheon `master`) — the normal path.
   - *Deploy to a multidev env* (feature env by branch name).
   - *Promote Dev → Test*.
   - *Promote Test → Live*.
   - *Full release*: Deploy to Dev → promote to Test → promote to Live → cut GitHub release.
3. **Cut a GitHub release at the end?** (yes/no) — only relevant once code is on Live.

Confirm the resolved plan back to the user in one line (site, `PANTHEON_NAME`, target env, release y/n) before proceeding.

## Step 1 — Preflight checks (🟢 you run; all must pass before deploying)

Run these and report a ✅/❌ checklist. **Stop and surface any ❌ — do not deploy past a failure.**

1. **Config matches the target site.** Read `.ddev/config.local.yaml` and confirm `EHEZA_SITE` and `PANTHEON_NAME` equal the chosen site's values.
   - If they don't match: tell the user to fix `.ddev/config.local.yaml` **and then run `ddev restart`** (web_environment vars only reload on restart). This is the #1 footgun — wrong `PANTHEON_NAME` pushes code to the **wrong Pantheon site**; wrong `EHEZA_SITE` builds the wrong site's data/config.
2. **`main` clean & level with `origin/main`.** Step 2 runs `git checkout main`, so you need not be sitting on `main` now — but it must be deploy-ready: in sync with the remote and free of *tracked* changes. The deploy aborts only on **tracked** changes; untracked files (e.g. `.ddev/`, `server/.pantheon-*`) are ignored, so check with `-uno` (not `-sb`, which would false-alarm on those):
   ```bash
   git fetch origin main --tags
   git rev-list --left-right --count main...origin/main   # expect "0	0" (level with origin/main)
   git status -s -uno                                     # expect empty (no tracked changes)
   ```
3. **All intended PRs merged into `main`.** Ask the user to confirm the release content is fully merged (you can show `git log --oneline origin/main -5` for context).
4. **Pantheon clone exists & clean.** Confirm `server/.pantheon-<PANTHEON_NAME>/` exists and is clean:
   ```bash
   git -C server/.pantheon-<PANTHEON_NAME> status -s -uno
   ```
   If the directory is missing, point the user to the one-time clone command in the wiki / reference file — they must create it before deploying.

## Step 2 — Local prep

- 🟢 You run: `git checkout main && git pull`.
- 🔴 User runs (paused): `ddev auth ssh` — authenticates to Pantheon over SSH (interactive).
- 🔴 User runs (paused): `ddev gulp publish` — minified production build of the Elm client. This is long; let it finish before deploying.

## Step 3 — Deploy

🔴 User runs (paused). Pick by target env:

- **Dev:** `ddev robo deploy:pantheon`
- **Multidev `<env>`:** `ddev robo deploy:pantheon <env>`

Tell the user explicitly: **at the "Commit changes and deploy?" prompt, review the printed `git status` of the Pantheon repo** and confirm only if the change-set is exactly what they expect.

What this command does automatically (so you don't double-run it): rsyncs the build into the Pantheon clone, commits + pushes, then runs on that env `cc all` (twice), `updb -y`, and `uli`. It does **NOT** run `fra`.

> Pantheon's `master` branch = the **Dev** environment, not production. Deploying to `master` only updates Dev.

## Step 4 — Post-deploy (every env you touched)

🔴 User runs (paused) — the one step the robo command skips:
```bash
ddev exec terminus remote:drush <PANTHEON_NAME>.<env> -- fra -y   # features revert all
```
Then sanity-check the env (open the `uli` login link from the deploy output, smoke-test the app). If the deploy added manual steps (new variables, migrations), run those too.

## Step 5 — Promote Dev → Test → Live (only for a full release / promotion)

Do these **in order**, pausing for the user and re-verifying between each. Each `deploy:pantheon-sync` runs `terminus env:deploy` then `cc all`×2 + `updb -y` + `uli` on that env (but again **not** `fra`).

1. 🔴 `ddev robo deploy:pantheon-sync test` → then Step 4 `fra` on `test` → verify on the Test URL.
2. Pause for explicit user go-ahead (this next one hits production).
3. 🔴 `ddev robo deploy:pantheon-sync live` → then Step 4 `fra` on `live` → verify on the Live URL.

(Equivalent GUI path: Pantheon dashboard → Test tab → confirm deploy → Live tab → confirm deploy.)

## Step 6 — Cut the GitHub release (only after code is on Live, if requested)

Run from the **eheza-app** repo (`origin` = `TIP-Global-Health/eheza-app`).

1. 🟢 Find the latest tag (this becomes the *previous* tag for the changelog):
   ```bash
   git fetch --tags origin
   git describe --tags --abbrev=0 origin/main
   ```
2. Decide the next tag **with the user**: patch bump (`v1.17.1 → v1.17.2`) for minor changes, minor bump (`→ v1.18.0`) for a new feature. Confirm before tagging.
3. 🟢 Create & push the new tag:
   ```bash
   git tag <new-tag> && git push origin <new-tag>
   ```
4. 🟢 Generate the changelog — **pass the PREVIOUS tag**, since `generate:release-notes` lists changes *since* the tag you give it:
   ```bash
   ddev robo generate:release-notes <previous-tag>
   ```
5. 🟢 Draft the release with `gh` (or hand the user the GitHub "new release" URL). Use the new tag, target `main`, title `Release <new-number>`. The body's first line states the deploy date and sites, e.g. `Deployed in production on <Month DD, YYYY> (<sites>)`, followed by the generator output starting at `## Changelog`:
   ```bash
   gh release create <new-tag> --target main --title "Release <new-number>" --notes "$(cat <<'EOF'
   Deployed in production on <Month DD, YYYY> (<sites>)

   <## Changelog ... output>
   EOF
   )"
   ```
   Confirm the date and site list with the user before publishing.

---

## Wrap-up

Report what was deployed: site, `PANTHEON_NAME`, each env reached (Dev/Test/Live), `fra` run per env, and the release tag/URL if cut. Note anything skipped or any preflight ❌ that blocked progress.

For deeper details (RoboFile internals, the one-time Pantheon clone setup, troubleshooting preflight failures), see `references/deploy-reference.md`.
