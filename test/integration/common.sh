# common.sh — shared setup for run.sh and dev.sh
# Source this file after setting SCRIPT_DIR and REPO_ROOT.
# Optional: set MATTERIRCD_LOG before sourcing to override the log path.

MM_URL="${MM_URL:-http://localhost:8065}"
IRC_HOST="${IRC_HOST:-127.0.0.1}"
IRC_PORT="${IRC_PORT:-6667}"
MATTERIRCD_VERSION="${MATTERIRCD_VERSION:-0.30.0}"
KEEP_SERVICES="${KEEP_SERVICES:-0}"

BIN_DIR="$SCRIPT_DIR/.bin"
MATTERIRCD_BIN="$BIN_DIR/matterircd"
MATTERIRCD_PID_FILE="$SCRIPT_DIR/.matterircd.pid"
LOG_DIR="$SCRIPT_DIR/.logs"
MATTERIRCD_LOG="${MATTERIRCD_LOG:-$LOG_DIR/matterircd.log}"
mkdir -p "$BIN_DIR" "$LOG_DIR"

# ── helpers ───────────────────────────────────────────────────────────
log()  { echo "[$(basename "$0")] $*"; }
die()  { echo "[$(basename "$0")] ERROR: $*" >&2; exit 1; }

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

# ── service lifecycle ─────────────────────────────────────────────────

download_matterircd() {
    [ -x "$MATTERIRCD_BIN" ] && return 0
    log "Downloading matterircd v${MATTERIRCD_VERSION}..."
    local asset_url
    asset_url=$(curl -sf \
        "https://api.github.com/repos/42wim/matterircd/releases/tags/v${MATTERIRCD_VERSION}" \
        | python3 -c "
import sys, json
assets = json.load(sys.stdin).get('assets', [])
for a in assets:
    n = a['name'].lower()
    if ('linux' in n) and ('amd64' in n or 'x86_64' in n) and not n.endswith('.sha256'):
        print(a['browser_download_url'])
        break
")
    [ -n "$asset_url" ] || die "Could not find linux/amd64 asset for matterircd v${MATTERIRCD_VERSION}"
    log "Downloading from $asset_url"
    if [[ "$asset_url" == *.tar.gz ]]; then
        curl -sL "$asset_url" | tar -xz -C "$BIN_DIR" matterircd
    else
        curl -sL "$asset_url" -o "$MATTERIRCD_BIN"
    fi
    chmod +x "$MATTERIRCD_BIN"
    log "matterircd downloaded: $("$MATTERIRCD_BIN" --version 2>&1 | head -1)"
}

start_mattermost() {
    log "Starting Mattermost..."
    docker compose -f "$SCRIPT_DIR/docker-compose.yml" up -d
    wait_for_http "$MM_URL/api/v4/system/ping" "Mattermost" 180
}

provision_mattermost() {
    log "Provisioning Mattermost..."
    eval "$(bash "$SCRIPT_DIR/setup-mattermost.sh")"
    export MM_ADMIN_TOKEN MM_TEAM_ID MM_TEST_USER_ID MM_CHANNEL_ID
}

start_matterircd() {
    log "Starting matterircd..."
    "$MATTERIRCD_BIN" \
        --conf "$SCRIPT_DIR/matterircd.toml" \
        --bind "$IRC_HOST:$IRC_PORT" \
        > "$MATTERIRCD_LOG" 2>&1 &
    echo $! > "$MATTERIRCD_PID_FILE"
    wait_for_tcp "$IRC_HOST" "$IRC_PORT" "matterircd" 30
}

# Stop matterircd and (unless KEEP_SERVICES=1) tear down Docker.
# Called from each script's own cleanup() trap.
cleanup_services() {
    if [ -f "$MATTERIRCD_PID_FILE" ]; then
        kill "$(cat "$MATTERIRCD_PID_FILE")" 2>/dev/null || true
        rm -f "$MATTERIRCD_PID_FILE"
    fi
    if [ "$KEEP_SERVICES" != "1" ]; then
        docker compose -f "$SCRIPT_DIR/docker-compose.yml" down -v --remove-orphans 2>/dev/null || true
    fi
}
