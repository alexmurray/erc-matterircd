#!/usr/bin/env bash
# Provision a fresh Mattermost instance for integration testing.
# Prints a shell snippet defining MM_ADMIN_TOKEN, MM_TEAM_ID,
# MM_TEST_USER_ID, MM_CHANNEL_ID so the caller can eval it.
set -euo pipefail

MM_URL="${MM_URL:-http://localhost:8065}"
ADMIN_USER="${MM_ADMIN_USER:-testadmin}"
ADMIN_PASS="${MM_ADMIN_PASS:-TestAdmin1!}"
ADMIN_EMAIL="${MM_ADMIN_EMAIL:-admin@test.local}"
TEAM_NAME="${MM_TEAM_NAME:-testteam}"
TEST_USER="${MM_TEST_USER:-testuser}"
TEST_PASS="${MM_TEST_PASS:-TestUser1!}"
TEST_EMAIL="${MM_TEST_EMAIL:-testuser@test.local}"
CHANNEL_NAME="${MM_CHANNEL_NAME:-testchannel}"

mm_api() {
    local method="$1" path="$2"
    shift 2
    curl -sf -X "$method" "$MM_URL/api/v4$path" \
        -H "Content-Type: application/json" \
        "$@"
}

mm_api_auth() {
    local method="$1" path="$2"
    shift 2
    mm_api "$method" "$path" -H "Authorization: Bearer $ADMIN_TOKEN" "$@"
}

# ── 1. Create admin (first user on a fresh instance needs no auth) ─
echo "Creating admin user..." >&2
mm_api POST /users -d "{
  \"email\":    \"$ADMIN_EMAIL\",
  \"username\": \"$ADMIN_USER\",
  \"password\": \"$ADMIN_PASS\"
}" > /dev/null || echo "(admin may already exist)" >&2

# ── 2. Login ───────────────────────────────────────────────────────
echo "Logging in as admin..." >&2
# The auth token is in the Token response header
ADMIN_TOKEN=$(curl -sfi -X POST "$MM_URL/api/v4/users/login" \
    -H "Content-Type: application/json" \
    -d "{\"login_id\":\"$ADMIN_USER\",\"password\":\"$ADMIN_PASS\"}" \
    | grep -i '^token:' | awk '{print $2}' | tr -d '\r\n')

if [ -z "$ADMIN_TOKEN" ]; then
    echo "ERROR: could not obtain admin token" >&2
    exit 1
fi
echo "Admin token obtained." >&2

# ── 3. Create team ─────────────────────────────────────────────────
echo "Creating team '$TEAM_NAME'..." >&2
TEAM_ID=$(mm_api_auth POST /teams -d "{
  \"name\":         \"$TEAM_NAME\",
  \"display_name\": \"Test Team\",
  \"type\":         \"O\"
}" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('id') or d.get('message',''))") || true

if [ -z "$TEAM_ID" ] || [ ${#TEAM_ID} -ne 26 ]; then
    # Team may already exist — look it up
    TEAM_ID=$(mm_api_auth GET "/teams/name/$TEAM_NAME" \
        | python3 -c "import sys,json; print(json.load(sys.stdin)['id'])")
fi
echo "Team ID: $TEAM_ID" >&2

# ── 4. Create test user ────────────────────────────────────────────
echo "Creating test user '$TEST_USER'..." >&2
TEST_USER_ID=$(mm_api_auth POST /users -d "{
  \"email\":    \"$TEST_EMAIL\",
  \"username\": \"$TEST_USER\",
  \"password\": \"$TEST_PASS\"
}" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('id') or d.get('message',''))") || true

if [ -z "$TEST_USER_ID" ] || [ ${#TEST_USER_ID} -ne 26 ]; then
    TEST_USER_ID=$(mm_api_auth GET "/users/username/$TEST_USER" \
        | python3 -c "import sys,json; print(json.load(sys.stdin)['id'])")
fi
echo "Test user ID: $TEST_USER_ID" >&2

# ── 5. Add test user to team ───────────────────────────────────────
mm_api_auth POST "/teams/$TEAM_ID/members" -d "{
  \"team_id\": \"$TEAM_ID\",
  \"user_id\": \"$TEST_USER_ID\"
}" > /dev/null
echo "Test user added to team." >&2

# ── 6. Create a dedicated test channel ────────────────────────────
echo "Creating channel '$CHANNEL_NAME'..." >&2
CHANNEL_ID=$(mm_api_auth POST /channels -d "{
  \"team_id\":      \"$TEAM_ID\",
  \"name\":         \"$CHANNEL_NAME\",
  \"display_name\": \"Test Channel\",
  \"type\":         \"O\"
}" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('id') or d.get('message',''))") || true

if [ -z "$CHANNEL_ID" ] || [ ${#CHANNEL_ID} -ne 26 ]; then
    CHANNEL_ID=$(mm_api_auth GET "/teams/$TEAM_ID/channels/name/$CHANNEL_NAME" \
        | python3 -c "import sys,json; print(json.load(sys.stdin)['id'])")
fi
echo "Channel ID: $CHANNEL_ID" >&2

# ── 7. Add test user to channel ────────────────────────────────────
mm_api_auth POST "/channels/$CHANNEL_ID/members" -d "{
  \"user_id\": \"$TEST_USER_ID\"
}" > /dev/null
echo "Test user added to channel." >&2

# ── Output env snippet for caller to eval ─────────────────────────
cat <<EOF
MM_ADMIN_TOKEN="$ADMIN_TOKEN"
MM_TEAM_ID="$TEAM_ID"
MM_TEST_USER_ID="$TEST_USER_ID"
MM_CHANNEL_ID="$CHANNEL_ID"
EOF
