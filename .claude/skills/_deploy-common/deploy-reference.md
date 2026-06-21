# Deploy reference — internals, setup, troubleshooting

Background detail shared by the `deploy-release` and `deploy-multidev` skills.
Source of truth: the wiki (https://github.com/Gizra/ihangane/wiki/Deployment)
and `server/RoboFile.php`.

## Pantheon sites

| `PANTHEON_NAME` | `EHEZA_SITE` | Clone dir (under `server/`) | Dashboard |
| --------------- | ------------ | --------------------------- | --------- |
| `ihangane`      | `rwanda`     | `.pantheon-ihangane`        | https://dashboard.pantheon.io/sites/ef0f3448-d0e5-4e83-b26c-875cfc77228b |
| `vhw`           | `burundi`    | `.pantheon-vhw`             | https://dashboard.pantheon.io/sites/7a63355a-86c4-4823-b9bd-145d24969627 |
| `tip-somalia`   | `somalia`    | `.pantheon-tip-somalia`     | https://dashboard.pantheon.io/sites/6e2186e4-348e-432d-bd54-25da6834cd12 |
| `uvl`           | `burundi`    | `.pantheon-uvl`             | https://dashboard.pantheon.io/sites/22e92340-dfa5-40cb-a5b8-58af49317149 |

`vhw` and `uvl` are two separate Burundi deployments — same `EHEZA_SITE`, different `PANTHEON_NAME`. Always confirm which one.

## What the robo commands actually do (`server/RoboFile.php`)

### `deployPantheon($branchName = 'master')` — `ddev robo deploy:pantheon [env]`
1. Resolves `PANTHEON_NAME` from the env var (falls back to the `PANTHEON_NAME` const, `eheza-app`, if unset — which is why the env var must be set correctly).
2. **Aborts if the eheza-app working tree is dirty.**
3. **Aborts if the Pantheon clone (`.pantheon-<name>`) is dirty.**
4. **Aborts if `pantheon.upstream.yml` is missing or has no `php_version:`.**
5. Checks out the target branch in the Pantheon clone (`master` for Dev, else the multidev branch name).
6. rsyncs `www/.` (server) and `client/dist/.` (app) into the clone, excluding `.git`, `.ddev`, `client`, etc.
7. Prints `git status`, then asks **"Commit changes and deploy?"** (interactive — needs a human).
8. On confirm: `git pull && git add . && git commit -am 'Site update' && git push` to Pantheon.
9. Calls `deployPantheonSync(<env>, FALSE)` → runs `cc all` (×2), `updb -y`, `uli` on that env. **Does not run `fra`.**

`$branchName == 'master'` maps to the Pantheon **`dev`** environment.

### `deployPantheonSync($env = 'test', $doDeploy = TRUE)` — `ddev robo deploy:pantheon-sync <env>`
- If `$doDeploy`: `terminus env:deploy <PANTHEON_NAME>.<env>` (promotes code from the previous env).
- Then always: `terminus remote:drush <env> -- cc all` (×2), `updb -y`, `uli`.
- Again, **no `fra`** — run it manually after.

### `generateReleaseNotes($tag = NULL)` — `ddev robo generate:release-notes [tag]`
- Lists changes **since** `$tag`. So `$tag` is the **previous** release tag (releasing `v1.17.2` → pass `v1.17.1`).
- If `$tag` is omitted it prompts to compare from the latest tag; pass it explicitly to avoid the prompt.
- Detects org/repo from `git remote get-url origin` to enrich entries via the GitHub API.

## `fra` is the manual post-deploy step

None of the robo commands run `drush fra` (features-revert-all). After every env you deploy/promote to:
```bash
ddev exec terminus remote:drush <PANTHEON_NAME>.<env> -- fra -y
```
`cc all` / `updb -y` / `uli` are already handled by the robo command for that env.

## One-time setup (prerequisites)

Per site, from inside `server/`, clone the Pantheon repo into `.pantheon-<PANTHEON_NAME>`:
```bash
# Rwanda (ihangane)
git clone ssh://codeserver.dev.ef0f3448-d0e5-4e83-b26c-875cfc77228b@codeserver.dev.ef0f3448-d0e5-4e83-b26c-875cfc77228b.drush.in:2222/~/repository.git -b master .pantheon-ihangane
# vhw
git clone ssh://codeserver.dev.7a63355a-86c4-4823-b9bd-145d24969627@codeserver.dev.7a63355a-86c4-4823-b9bd-145d24969627.drush.in:2222/~/repository.git -b master .pantheon-vhw
# tip-somalia
git clone ssh://codeserver.dev.6e2186e4-348e-432d-bd54-25da6834cd12@codeserver.dev.6e2186e4-348e-432d-bd54-25da6834cd12.drush.in:2222/~/repository.git -b master .pantheon-tip-somalia
# uvl
git clone ssh://codeserver.dev.22e92340-dfa5-40cb-a5b8-58af49317149@codeserver.dev.22e92340-dfa5-40cb-a5b8-58af49317149.drush.in:2222/~/repository.git -b master .pantheon-uvl
```
Also required (one-time): a Pantheon team membership + SSH key on the Pantheon account, and the `.ddev/config.local.yaml` `web_environment` entries (`EHEZA_SITE`, `PANTHEON_NAME`, and for infra also `EHEZA_INFRA_REPO_REMOTE`, `GITHUB_USERNAME`, `GITHUB_ACCESS_TOKEN`).

**`TERMINUS_MACHINE_TOKEN` (required for deploys).** SSH keys cover the git *push* to Pantheon, but `terminus` — used for the post-deploy `remote:drush` `cc all`/`updb`/`fra` steps — has its own auth. Create a machine token at https://dashboard.pantheon.io/machine-token/create (shown once; copy it), then add it to `web_environment` in `.ddev/config.local.yaml` so terminus auto-authenticates: `- TERMINUS_MACHINE_TOKEN=<token>` (then `ddev restart`). Without it, the deploy pushes code but every `remote:drush` step fails with *"You are not logged in"*, leaving the env on new code with a stale cache/registry.

## Reinstall-on-restart toggle (`config.local.yaml` → `post-start`)

`ddev restart` re-runs the `post-start` hook. By default only one line there is active —
`- exec-host: ddev client-install` (installs the Elm client deps; **not** a reinstall) — and
everything below it is commented out, so **a normal restart does not reinstall** the local site.
The deploy skills run this restart on **every** deploy (Step 2) and always ask the reinstall
question first — default **No**.

To make a restart **reinstall the project**, uncomment the *entire* commented `post-start` block —
from `- exec: "cd .. && chmod +x ./scripts/build && ./scripts/build"` down through the final
`- exec: drush uli`. That runs the full local rebuild for the selected `$EHEZA_SITE`: build, copy
the site CSVs, `drush site-install`, enable modules, run the `default`/`counseling`/`forms`
migrations, and set feature flags. Re-comment the block afterward if you don't want every future
restart to reinstall.

A reinstall is usually **not** needed to deploy. The deploy builds the client (`gulp publish`) and
rsyncs `www/` independent of the local DB, and `www/profiles/hedley` is a **symlink** → `server/hedley`,
which the deploy's `rsync -L` follows — so the **custom** E-Heza server code always reflects the
checked-out branch live, no rebuild required.

The one exception: `www/`'s Drupal **core + contrib** are assembled by `server/scripts/build`
(`drush make`) and are **copied**, not symlinked. So if the branch you're deploying changed
`drupal-org.make` / `drupal-org-core.make` (contrib versions) versus the currently-built `www/`,
those are stale and only a **reinstall** (which re-runs `scripts/build`) refreshes them. For branches
that didn't touch contrib, deploy as-is. Reinstall also when you want your local environment to
reflect the newly-selected site's data.

## Troubleshooting preflight / deploy failures

- **"The GitHub working directory is dirty"** — commit/stash/`.gitignore` pending changes in the eheza-app repo, then retry.
- **"The Pantheon working directory is dirty"** — `cd server/.pantheon-<name> && git status`; the deploy offers to `git checkout . && git clean -fd` if you decline the commit. Clean it before retrying.
- **"pantheon.upstream.yml is missing / php_version directive is missing"** — the Pantheon clone is stale or wrong; re-pull it (`git -C server/.pantheon-<name> pull`).
- **Pushed to the wrong site** — caused by a stale `PANTHEON_NAME`. Fix `.ddev/config.local.yaml` and **`ddev restart`** (env vars only reload on restart) before re-deploying.
- **Empty changelog** — you passed the *new* tag to `generate:release-notes` instead of the previous one.
- **SSH auth errors (git push to Pantheon)** — re-run `ddev auth ssh`; confirm Pantheon team membership and that your SSH key is on the Pantheon account.
- **`terminus` "You are not logged in"** — `ddev auth ssh` does **not** authenticate terminus. Set `TERMINUS_MACHINE_TOKEN` in `.ddev/config.local.yaml` (see prerequisites) and `ddev restart`, or run **`ddev terminus-auth`** (logs terminus in from the token; or `ddev terminus-auth <token>`); verify with `ddev exec terminus auth:whoami`. The `robo` deploy may already have **pushed the code** before failing here — so after authenticating, just re-run the post-deploy `remote:drush` steps (`cc all` ×2, `updb -y`, `uli`, `fra -y`) rather than the whole deploy.

## Branch note

The eheza-app default branch is `main`; releases are tagged on `main` and the release target is `main`.
