#!/usr/bin/env bash
# Delta sync — paste into Jenkins Freestyle project "Execute shell" build step.
#
# Jenkins Freestyle setup:
#   1. Build Environment → "Use secret text(s) or file(s)":
#      - Secret file: PG_CLIENT_CERT  → credential id: pg-client-cert
#      - Secret file: PG_CLIENT_KEY   → credential id: pg-client-key
#      - Secret file: PG_SERVER_CA    → credential id: pg-server-ca
#      - Secret text: PG_PASSWORD     → credential id: pg-password
#   2. General → "This project is parameterized":
#      - Choice Parameter: name=MODE, choices=delta\nfull-seed
#   3. Build Triggers → "Build periodically": 0 3 * * *
#   4. Build → "Execute shell": paste this script
#
# All configuration is in the block below — adjust as needed.

set -euo pipefail

# === Configuration ============================================================
PANTHEON_SITE="ihangane"
PANTHEON_ENV="aos-backend"
EXPORT_SITE="rwanda"
DRUPAL_SCRIPT_DIR="profiles/hedley/modules/custom/hedley_migrate/data-pipeline/scripts"

PG_HOST="34.32.23.190"
PG_PORT="5432"
PG_DB="encounters"
PG_USER="encounters-admin"

DELTA_DIR="/tmp/delta-sync"
RETENTION_DAYS=30
# ==============================================================================

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"; }

die() {
  log "ERROR: $*" >&2
  if [[ -n "${SYNC_ID:-}" ]]; then
    psql "$CONNSTR" -q -c \
      "UPDATE sync_state SET status='failed', completed_at=NOW(),
       error_message='$(echo "$*" | sed "s/'/''/g")'
       WHERE sync_id=$SYNC_ID;" 2>/dev/null || true
  fi
  exit 1
}

# --- Build connection string --------------------------------------------------
if [[ -z "${PG_PASSWORD:-}" ]]; then
  die "PG_PASSWORD not set. Check Jenkins credential bindings."
fi

# Fix key permissions (Jenkins extracts with broad perms)
if [[ -n "${PG_CLIENT_KEY:-}" && -f "$PG_CLIENT_KEY" ]]; then
  chmod 600 "$PG_CLIENT_KEY"
fi

export PGPASSWORD="$PG_PASSWORD"
CONNSTR="host=$PG_HOST port=$PG_PORT dbname=$PG_DB user=$PG_USER"
if [[ -n "${PG_CLIENT_CERT:-}" && -f "$PG_CLIENT_CERT" ]]; then
  CONNSTR="$CONNSTR sslmode=verify-ca"
  export PGSSLCERT="$PG_CLIENT_CERT"
  export PGSSLKEY="$PG_CLIENT_KEY"
  export PGSSLROOTCERT="$PG_SERVER_CA"
else
  CONNSTR="$CONNSTR sslmode=require"
fi

# --- Full seed mode -----------------------------------------------------------
if [[ "${MODE:-delta}" == "full-seed" ]]; then
  log "Seeding sync_state after full export..."
  MAX_VID=$(terminus drush "$PANTHEON_SITE.$PANTHEON_ENV" -- sqlq \
    "SELECT MAX(vid) FROM node_revision" 2>/dev/null | tr -d '[:space:]')

  if [[ -z "$MAX_VID" || "$MAX_VID" == "NULL" ]]; then
    die "Could not retrieve max vid from Drupal"
  fi

  psql "$CONNSTR" -c \
    "INSERT INTO sync_state (sync_type, started_at, completed_at, status, last_vid)
     VALUES ('full', NOW(), NOW(), 'success', $MAX_VID);"

  log "Seeded sync_state with last_vid=$MAX_VID"
  exit 0
fi

# --- Clone live DB to multidev ------------------------------------------------
log "Cloning live database to $PANTHEON_ENV..."
terminus env:clone-content "$PANTHEON_SITE.live" "$PANTHEON_ENV" --db-only --yes \
  || die "Failed to clone live database to $PANTHEON_ENV"
log "Database clone complete."

# --- Get last successful vid --------------------------------------------------
log "Querying last successful sync..."
LAST_VID=$(psql "$CONNSTR" -t -A -c \
  "SELECT COALESCE(MAX(last_vid), -1) FROM sync_state WHERE status='success';")

if [[ "$LAST_VID" == "-1" ]]; then
  die "No previous successful sync found. Run full export + seed sync_state first."
fi

log "Last successful vid: $LAST_VID"

# --- Insert running sync record -----------------------------------------------
SYNC_ID=$(psql "$CONNSTR" -t -A -c \
  "INSERT INTO sync_state (sync_type, started_at, status)
   VALUES ('delta', NOW(), 'running') RETURNING sync_id;" | head -1 | tr -d '[:space:]')

log "Created sync record #$SYNC_ID"

# --- Run delta export on Pantheon ---------------------------------------------
mkdir -p "$DELTA_DIR"
DELTA_FILE="$DELTA_DIR/delta-$(date '+%Y%m%d-%H%M%S').sql"

log "Running delta export (since vid $LAST_VID)..."
terminus drush "$PANTHEON_SITE.$PANTHEON_ENV" -- \
  scr "$DRUPAL_SCRIPT_DIR/export-delta.php" --since-vid="$LAST_VID" --site="$EXPORT_SITE" \
  > "$DELTA_FILE" 2>/tmp/delta-export-stderr.log

# Check for PHP errors in output
if grep -qiE '(Fatal error|Parse error|Warning:|Notice:|Drush command terminated)' "$DELTA_FILE"; then
  die "PHP errors detected in delta export output. Check $DELTA_FILE"
fi

if grep -qiE '(Fatal error|Parse error)' /tmp/delta-export-stderr.log 2>/dev/null; then
  die "PHP errors on stderr. Check /tmp/delta-export-stderr.log"
fi

# Extract metadata from SQL comments
MAX_VID_NEW=$(grep -- '-- max_vid:' "$DELTA_FILE" | sed 's/.*-- max_vid: //' | tr -d '[:space:]')
ENCOUNTERS=$(grep -- '-- encounters_processed:' "$DELTA_FILE" | sed 's/.*-- encounters_processed: //' | tr -d '[:space:]')
CHILDREN=$(grep -- '-- children_processed:' "$DELTA_FILE" | sed 's/.*-- children_processed: //' | tr -d '[:space:]')
ENCOUNTERS=${ENCOUNTERS:-0}
CHILDREN=${CHILDREN:-0}

if [[ -z "$MAX_VID_NEW" ]]; then
  die "Could not extract max_vid from delta export output"
fi

log "Delta: $ENCOUNTERS encounters, $CHILDREN children, new max_vid=$MAX_VID_NEW"

# --- Load delta into PostgreSQL -----------------------------------------------
FILESIZE=$(stat -c%s "$DELTA_FILE" 2>/dev/null || echo "0")
log "Loading delta SQL ($FILESIZE bytes)..."

psql "$CONNSTR" -v ON_ERROR_STOP=1 -f "$DELTA_FILE" \
  || die "Failed to load delta SQL into PostgreSQL"

# --- Mark sync as successful --------------------------------------------------
psql "$CONNSTR" -q -c \
  "UPDATE sync_state
   SET status='success', completed_at=NOW(), last_vid=$MAX_VID_NEW,
       encounters_processed=$ENCOUNTERS, children_processed=$CHILDREN
   WHERE sync_id=$SYNC_ID;"

log "Sync #$SYNC_ID completed successfully (vid $LAST_VID → $MAX_VID_NEW)"

# --- Cleanup old delta files --------------------------------------------------
find "$DELTA_DIR" -name 'delta-*.sql' -mtime +$RETENTION_DAYS -delete 2>/dev/null || true

log "Done."
