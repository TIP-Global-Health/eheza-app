# OpenFN integration — provisioning runbook

The OpenFN Lightning workflow for the E-Heza → OpenMRS patient PoC. A person
registered in E-Heza is POSTed to a webhook, which runs three jobs in order:

```
webhook → transform → match → load
```

- **transform** (`jobs/transform.js`) — E-Heza person → OpenMRS patient body.
- **match** (`jobs/match.js`) — find an existing OpenMRS patient (link) or not (create).
- **load** (`jobs/load.js`) — create the patient when needed, write the UUID back to E-Heza.

See `../patient-field-mapping.md`, `../identity-matching.md`, `../load-step.md`.

## Files

| File | |
|------|--|
| `docker-compose.yml` | local Lightning stack (web + worker + postgres) |
| `project.yaml` | the deployable workflow definition |
| `jobs/*.js` | the three job scripts (unit tests: `jobs/*.test.js`) |
| `.env` | Lightning stack secrets — gitignored |
| `.config.json` | openfn CLI deploy config (endpoint + API key) — gitignored |
| `.state.json` | openfn deploy state — gitignored |

## Prerequisites

- Lightning stack up: `docker compose up -d` (UI: http://localhost:4001)
- Local OpenMRS up: see `../openmrs`
- DDEV up: the E-Heza backend

## Provisioning

### 1. Lightning account + API token

A fresh Lightning instance has no users. Create a superuser, then an API token:

```bash
docker compose exec web /app/bin/lightning rpc \
  'Lightning.Accounts.register_superuser(%{first_name: "PoC", last_name: "Admin", email: "poc@openfn.local", password: "<password>"})'

docker compose exec web /app/bin/lightning rpc \
  'u = Lightning.Repo.get_by!(Lightning.Accounts.User, email: "poc@openfn.local"); IO.puts(Lightning.Accounts.generate_api_token(u))'
```

Put the endpoint and token in `.config.json`:

```json
{ "endpoint": "http://localhost:4001", "apiKey": "<token>" }
```

### 2. Deploy the workflow

```bash
npx @openfn/cli deploy -c .config.json -p project.yaml --state-path .state.json -y
```

This creates the `eheza-openmrs` project — the webhook trigger and the three
jobs. Re-run it after editing any `jobs/*.js` or `project.yaml`.

### 3. Credentials

`match` and `load` read `state.configuration` from a Lightning credential.
Secrets are not carried in `project.yaml`; configure it through the
Lightning UI after the deploy:

1. **Create the credential.** In project settings
   (`/projects/<project-id>/settings#credentials`) → New → **Raw JSON**,
   name `eheza-openmrs-config`. **Environment matters in Lightning v2.x:**
   the credential's environment must match the project's. A fresh project
   has no env set, which Lightning calls `unknown` — set the credential's
   environment to `unknown` accordingly. Body:

   ```json
   {
     "openmrsBaseUrl": "http://host.docker.internal:8090/openmrs/ws/rest/v1",
     "openmrsAuth": "Basic <base64 of the OpenMRS integration user:password>",
     "ehezaPatientLinkUrl": "http://ddev-ihangane-web/openmrs/patient-link",
     "ehezaToken": "<value of the Drupal hedley_openmrs_shared_secret variable>"
   }
   ```

   The E-Heza URL uses the DDEV web container name because the worker
   joins DDEV's network (see `docker-compose.yml`).

2. **Attach to the steps.** Open the `Patient sync` workflow editor and,
   on the **Match** and **Load** steps, open *Configure connection* and
   pick `eheza-openmrs-config`. Transform needs none.

3. **Save the workflow.** Use the workflow-level Save (not the modal's
   Close) — that commits the attachment *and cuts a fresh snapshot*. Runs
   created before this save use the older snapshot and ignore the
   credential. Verified once end-to-end: registration → queue → webhook →
   transform → match → load → patient in OpenMRS + UUID written back.

### 4. Point E-Heza at the webhook

The webhook URL is `<lightning>/i/<trigger-id>`. Set it on the Drupal side so
the `hedley_openmrs` queue worker posts there:

```bash
ddev drush vset hedley_openmrs_openfn_webhook_url \
  'http://host.docker.internal:4001/i/<trigger-id>'
```

Currently deployed trigger: `deaf5f93-e554-44c5-b006-7ac77636c3b9`.

## Stack wiring notes

Two `docker-compose.yml` quirks worth understanding (both baked in):

- **Worker networking** — the worker joins DDEV's network (so the load
  job can reach the E-Heza backend by container name) and reaches OpenMRS
  via `host.docker.internal`. It targets Lightning by container name,
  because `web` is ambiguous once the worker is on DDEV's network.
- **`ORIGINS`** — required on the Lightning service, else the worker
  websocket's `check_origin` is `nil` and the connection crashes.

The Lightning webhook trigger is unauthenticated by default; if a token is
added, also set the Drupal `hedley_openmrs_openfn_token` variable.
