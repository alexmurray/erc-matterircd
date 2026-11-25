#!/usr/bin/env bash
# Start a local Mattermost + matterircd instance and launch Emacs for
# interactive testing of erc-matterircd.
#
# Usage:
#   ./test/integration/dev.sh                    # start services, launch Emacs
#   KEEP_SERVICES=1 ./test/integration/dev.sh    # leave services running on exit
#
# The same provisioned test accounts used by the automated integration tests
# are reused here (testadmin / testuser / testteam / #testchannel).  Emacs
# auto-connects to matterircd on startup with those credentials.
#
# Environment overrides (all optional):
#   MM_URL              Mattermost base URL       (default: http://localhost:8065)
#   IRC_HOST            matterircd bind address   (default: 127.0.0.1)
#   IRC_PORT            matterircd IRC port        (default: 6667)
#   MATTERIRCD_VERSION  matterircd release tag     (default: 0.30.0)
#   KEEP_SERVICES       set to 1 to keep Docker + matterircd alive after exit
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

MATTERIRCD_LOG="${MATTERIRCD_LOG:-$SCRIPT_DIR/.logs/matterircd-dev.log}"

# shellcheck source=test/integration/common.sh
source "$SCRIPT_DIR/common.sh"

cleanup() {
    log "Cleaning up..."
    cleanup_services
}
trap cleanup EXIT

download_matterircd
start_mattermost
provision_mattermost
start_matterircd

# ── Export variables for dev-init.el ─────────────────────────────────
MM_TEAM="${MM_TEAM_NAME:-testteam}"
MM_NICK="${MM_TEST_USER:-testuser}"
MM_PASS="${MM_TEST_PASS:-TestUser1!}"
MM_CHANNEL="${MM_CHANNEL_NAME:-testchannel}"
export REPO_ROOT IRC_HOST IRC_PORT MM_URL MATTERIRCD_LOG
export MM_TEAM MM_NICK MM_PASS MM_CHANNEL

# ── Open Mattermost in browser ────────────────────────────────────────
MM_ADMIN_USER="${MM_ADMIN_USER:-testadmin}"
MM_ADMIN_PASS="${MM_ADMIN_PASS:-TestAdmin1!}"
CHANNEL_URL="$MM_URL/$MM_TEAM/channels/$MM_CHANNEL"
xdg-open "$CHANNEL_URL" 2>/dev/null || true

# ── Launch Emacs ──────────────────────────────────────────────────────
log ""
log "  Mattermost UI : $CHANNEL_URL"
log "    ERC user    : $MM_NICK / $MM_PASS"
log "    Admin user  : $MM_ADMIN_USER / $MM_ADMIN_PASS"
log "  IRC           : $IRC_HOST:$IRC_PORT"
log ""
log "Launching Emacs — quit Emacs (C-x C-c) to tear down services."
log "Set KEEP_SERVICES=1 to leave Mattermost and matterircd running afterwards."
log ""

emacs --no-init-file --load "$SCRIPT_DIR/dev-init.el"
