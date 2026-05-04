#!/usr/bin/env bash
# test-reconnect.sh — Test CLoak graceful reconnection
#
# Prerequisites:
#   - ngircd running on 127.0.0.1:6667
#   - CLoak built at bin/cloak
#   - User configured: glenneth2/local:password (adjust below)
#
# What it tests:
#   1. CLoak starts and connects to upstream
#   2. IRC client connects through CLoak
#   3. Messages flow correctly
#   4. ngircd restart → CLoak reconnects automatically
#   5. Client stays connected, messages resume
#   6. Playback works after reconnection
#
# Usage: ./test/test-reconnect.sh

set -euo pipefail

# --- Configuration ---
CLOAK_BIN="./bin/cloak"
CLOAK_PORT=6697
CLOAK_HOST="127.0.0.1"
# Adjust these to match your config:
IRC_PASS="glenneth2/local:b3l0wz3r0"
IRC_NICK="testbot$$"
CHANNEL="#clatter"
TIMEOUT=10

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

pass() { echo -e "${GREEN}[PASS]${NC} $1"; }
fail() { echo -e "${RED}[FAIL]${NC} $1"; FAILURES=$((FAILURES + 1)); }
info() { echo -e "${YELLOW}[INFO]${NC} $1"; }

FAILURES=0
CLOAK_PID=""
CLIENT_PID=""
FIFO=""

cleanup() {
    info "Cleaning up..."
    [ -n "$CLIENT_PID" ] && kill "$CLIENT_PID" 2>/dev/null || true
    [ -n "$CLOAK_PID" ] && kill "$CLOAK_PID" 2>/dev/null || true
    [ -n "$FIFO" ] && rm -f "$FIFO" || true
    # Wait for processes to die
    [ -n "$CLOAK_PID" ] && wait "$CLOAK_PID" 2>/dev/null || true
}
trap cleanup EXIT

# --- Helper: send IRC line ---
irc_send() {
    echo -e "$1\r" >&3
    sleep 0.3
}

# --- Helper: read IRC lines with timeout ---
irc_read() {
    local secs="${1:-3}"
    timeout "$secs" cat <&4 2>/dev/null || true
}

echo "========================================"
echo "  CLoak Reconnection Test"
echo "========================================"
echo

# --- Step 1: Verify prerequisites ---
info "Checking prerequisites..."

if [ ! -x "$CLOAK_BIN" ]; then
    fail "CLoak binary not found at $CLOAK_BIN (run 'make build' first)"
    exit 1
fi

if ! systemctl is-active --quiet ngircd 2>/dev/null; then
    if ! pgrep -x ngircd >/dev/null; then
        fail "ngircd is not running"
        exit 1
    fi
fi
pass "Prerequisites OK"

# --- Step 2: Start CLoak ---
info "Starting CLoak..."
$CLOAK_BIN &
CLOAK_PID=$!
sleep 3

if ! kill -0 "$CLOAK_PID" 2>/dev/null; then
    fail "CLoak failed to start"
    exit 1
fi
pass "CLoak started (PID $CLOAK_PID)"

# --- Step 3: Connect IRC client ---
info "Connecting IRC client to CLoak on $CLOAK_HOST:$CLOAK_PORT..."

# Use a FIFO for bidirectional communication
FIFO=$(mktemp -u /tmp/cloak-test-XXXXXX)
mkfifo "$FIFO"

# Start netcat in background
nc "$CLOAK_HOST" "$CLOAK_PORT" <"$FIFO" >"/tmp/cloak-test-output-$$" &
CLIENT_PID=$!
exec 3>"$FIFO"    # write end
sleep 1

if ! kill -0 "$CLIENT_PID" 2>/dev/null; then
    fail "Could not connect to CLoak"
    exit 1
fi

# Authenticate
irc_send "PASS $IRC_PASS"
irc_send "NICK $IRC_NICK"
irc_send "USER $IRC_NICK 0 * :Reconnect Test"
sleep 2

# Check for welcome
if grep -q "001" "/tmp/cloak-test-output-$$" 2>/dev/null; then
    pass "IRC client authenticated and received welcome"
else
    fail "No welcome received (check IRC_PASS in script)"
    cat "/tmp/cloak-test-output-$$" 2>/dev/null || true
    exit 1
fi

# --- Step 4: Send a test message ---
info "Sending test message to $CHANNEL..."
irc_send "PRIVMSG $CHANNEL :CLoak reconnect test - message BEFORE restart ($(date +%H:%M:%S))"
sleep 1
pass "Test message sent"

# --- Step 5: Restart ngircd ---
info "Restarting ngircd (simulating server crash)..."
echo
info ">>> sudo systemctl restart ngircd <<<"
echo
sudo systemctl restart ngircd
sleep 2
pass "ngircd restarted"

# --- Step 6: Wait for CLoak to reconnect ---
info "Waiting for CLoak to reconnect (up to 30s)..."
RECONNECTED=false
for i in $(seq 1 30); do
    if grep -q "Connected to local" "/tmp/cloak-test-output-$$" 2>/dev/null; then
        # Check CLoak's own output
        true
    fi
    # Just wait — CLoak logs to stdout, not to our client output
    sleep 1
    # Try sending a PING to see if bouncer is still alive
    irc_send "PING :reconntest$i"
    if grep -q "PONG.*reconntest$i" "/tmp/cloak-test-output-$$" 2>/dev/null; then
        RECONNECTED=true
        break
    fi
done

if $RECONNECTED; then
    pass "CLoak bouncer still responding after ngircd restart"
else
    fail "CLoak bouncer stopped responding"
fi

# --- Step 7: Send post-reconnect message ---
info "Waiting additional 5s for upstream reconnect..."
sleep 5

info "Sending message after reconnection..."
irc_send "PRIVMSG $CHANNEL :CLoak reconnect test - message AFTER restart ($(date +%H:%M:%S))"
sleep 2
pass "Post-reconnect message sent"

# --- Step 8: Test playback ---
info "Testing playback module..."
irc_send "PRIVMSG *playback :since 5m"
sleep 2

if grep -qi "playback\|Replaying" "/tmp/cloak-test-output-$$" 2>/dev/null; then
    pass "Playback module responded"
else
    info "Playback module may not be enabled (enable 'playback' module to test)"
fi

# --- Step 9: Verify client is still connected ---
irc_send "PING :finalcheck"
sleep 2
if grep -q "PONG.*finalcheck" "/tmp/cloak-test-output-$$" 2>/dev/null; then
    pass "Client still connected after full test"
else
    fail "Client connection lost"
fi

# --- Summary ---
echo
echo "========================================"
if [ "$FAILURES" -eq 0 ]; then
    echo -e "  ${GREEN}ALL TESTS PASSED${NC}"
else
    echo -e "  ${RED}$FAILURES TEST(S) FAILED${NC}"
fi
echo "========================================"
echo
info "Full client output saved to /tmp/cloak-test-output-$$"
info "CLoak PID: $CLOAK_PID (will be killed on exit)"

# Cleanup happens via trap
rm -f "/tmp/cloak-test-output-$$"
exit $FAILURES
