#!/usr/bin/env bash
# Run the erc-matterircd integration test suite against real
# Mattermost + matterircd services.
#
# Usage:
#   ./test/integration/run.sh            # start services, run tests, teardown
#   KEEP_SERVICES=1 ./test/integration/run.sh  # leave services running afterwards
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

MM_URL="${MM_URL:-http://localhost:8065}"
IRC_HOST="${IRC_HOST:-127.0.0.1}"
IRC_PORT="${IRC_PORT:-6667}"
MATTERIRCD_VERSION="${MATTERIRCD_VERSION:-0.30.0}"
KEEP_SERVICES="${KEEP_SERVICES:-0}"

BIN_DIR="$SCRIPT_DIR/.bin"
MATTERIRCD_BIN="$BIN_DIR/matterircd"
MATTERIRCD_PID_FILE="$SCRIPT_DIR/.matterircd.pid"
LOG_DIR="$SCRIPT_DIR/.logs"
mkdir -p "$BIN_DIR" "$LOG_DIR"

# ── helpers ───────────────────────────────────────────────────────
log()  { echo "[run.sh] $*"; }
die()  { echo "[run.sh] ERROR: $*" >&2; exit 1; }

wait_for_http() {
    local url="$1" label="$2" timeout="${3:-120}"
    log "Waiting for $label at $url ..."
    local deadline=$(( $(date +%s) + timeout ))
    until curl -sf "$url" > /dev/null 2>&1; do
        [ "$(date +%s)" -lt "$deadline" ] || die "$label did not become ready in ${timeout}s"
        sleep 3
    done
    log "$label is ready."
}

wait_for_tcp() {
    local host="$1" port="$2" label="$3" timeout="${4:-30}"
    log "Waiting for $label on $host:$port ..."
    local deadline=$(( $(date +%s) + timeout ))
    until nc -z "$host" "$port" 2>/dev/null; do
        [ "$(date +%s)" -lt "$deadline" ] || die "$label did not open $host:$port in ${timeout}s"
        sleep 1
    done
    log "$label is ready."
}

cleanup() {
    log "Cleaning up..."
    if [ -f "$MATTERIRCD_PID_FILE" ]; then
        kill "$(cat "$MATTERIRCD_PID_FILE")" 2>/dev/null || true
        rm -f "$MATTERIRCD_PID_FILE"
    fi
    if [ "$KEEP_SERVICES" != "1" ]; then
        docker compose -f "$SCRIPT_DIR/docker-compose.yml" down -v --remove-orphans 2>/dev/null || true
    fi
}
trap cleanup EXIT

# ── 1. Download matterircd if needed ──────────────────────────────
if [ ! -x "$MATTERIRCD_BIN" ]; then
    log "Downloading matterircd v${MATTERIRCD_VERSION}..."
    # Fetch the actual download URL for the linux amd64 asset from the GitHub API
    ASSET_URL=$(curl -sf \
        "https://api.github.com/repos/42wim/matterircd/releases/tags/v${MATTERIRCD_VERSION}" \
        | python3 -c "
import sys, json
assets = json.load(sys.stdin).get('assets', [])
for a in assets:
    n = a['name'].lower()
    if ('linux' in n or 'linux' in n) and ('amd64' in n or 'x86_64' in n) and not n.endswith('.sha256'):
        print(a['browser_download_url'])
        break
")
    [ -n "$ASSET_URL" ] || die "Could not find linux/amd64 asset for matterircd v${MATTERIRCD_VERSION}"
    log "Downloading from $ASSET_URL"
    # Asset may be a raw binary or a .tar.gz
    if [[ "$ASSET_URL" == *.tar.gz ]]; then
        curl -sL "$ASSET_URL" | tar -xz -C "$BIN_DIR" matterircd
    else
        curl -sL "$ASSET_URL" -o "$MATTERIRCD_BIN"
    fi
    chmod +x "$MATTERIRCD_BIN"
    log "matterircd downloaded: $("$MATTERIRCD_BIN" --version 2>&1 | head -1)"
fi

# ── 2. Start Mattermost ───────────────────────────────────────────
log "Starting Mattermost..."
docker compose -f "$SCRIPT_DIR/docker-compose.yml" up -d
wait_for_http "$MM_URL/api/v4/system/ping" "Mattermost" 180

# ── 3. Provision test users/team/channel ─────────────────────────
log "Provisioning Mattermost..."
eval "$(bash "$SCRIPT_DIR/setup-mattermost.sh")"
# Now we have: MM_ADMIN_TOKEN, MM_TEAM_ID, MM_TEST_USER_ID, MM_CHANNEL_ID
export MM_ADMIN_TOKEN MM_TEAM_ID MM_TEST_USER_ID MM_CHANNEL_ID

# ── 4. Start matterircd ───────────────────────────────────────────
log "Starting matterircd..."
"$MATTERIRCD_BIN" \
    --conf "$SCRIPT_DIR/matterircd.toml" \
    --bind "$IRC_HOST:$IRC_PORT" \
    > "$LOG_DIR/matterircd.log" 2>&1 &
echo $! > "$MATTERIRCD_PID_FILE"
wait_for_tcp "$IRC_HOST" "$IRC_PORT" "matterircd" 30

# ── 5. Run Emacs integration tests ───────────────────────────────
log "Running Emacs integration tests..."
# Use a clean throwaway HOME so local ~/.authinfo, ~/.emacs.d, snap
# site-start.el, etc. cannot interfere — matching the CI environment.
EMACS_HOME=$(mktemp -d)
set +e
HOME="$EMACS_HOME" emacs --batch --no-init-file \
    -L "$REPO_ROOT" \
    --eval "(setq debug-on-error t)" \
    -l "$SCRIPT_DIR/erc-matterircd-integration-test.el" \
    --eval "(emt-run)" \
    2>&1 | tee "$LOG_DIR/emacs-test.log"
EXIT_CODE=${PIPESTATUS[0]}
rm -rf "$EMACS_HOME"
set -e

if [ "$EXIT_CODE" -eq 0 ]; then
    log "Integration tests PASSED."
else
    log "Integration tests FAILED (exit $EXIT_CODE). Logs:"
    echo "--- matterircd log ---"
    cat "$LOG_DIR/matterircd.log"
    echo "--- emacs log ---"
    cat "$LOG_DIR/emacs-test.log"
fi

exit "$EXIT_CODE"
