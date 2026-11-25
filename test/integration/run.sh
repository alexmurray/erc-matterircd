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

# ── Run Emacs integration tests ───────────────────────────────────────
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
    cat "$MATTERIRCD_LOG"
    echo "--- emacs log ---"
    cat "$LOG_DIR/emacs-test.log"
fi

exit "$EXIT_CODE"
