#!/bin/bash
# smoke.sh -- the agent/sxk guard. Isolated session dir, every verb
# exercised, including the failure paths (try timeout, external kernel
# kill, again-recovery). Usage: tests/smoke.sh [15|13]   (default 15)
set -uo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
SXK="$HERE/sxk"
export SXK_DIR="/tmp/sxk-smoke-$USER-$$"
K="${1:-15}"

pass=0; fail=0
ok()  { pass=$((pass+1)); echo "  ok  - $1"; }
bad() { fail=$((fail+1)); echo "  FAIL- $1"; }
check() {  # check <desc> <expected-substring> <actual>
    case "$3" in *"$2"*) ok "$1" ;; *) bad "$1 (wanted '$2', got: $(echo "$3" | head -3))" ;; esac
}

echo "== agent/sxk smoke on kernel $K, session $SXK_DIR =="

r=$("$SXK" start "$K" 2>&1);                    check "start"            "kernel $K up" "$r"
r=$("$SXK" status 2>&1);                        check "status idle"      "idle" "$r"
r=$("$SXK" do '2+2' 2>&1);                      check "do arithmetic"    "4" "$r"
r=$("$SXK" do 'smokeX = 7; Print["set ", smokeX]' 2>&1); check "do with Print" 'set "7' "$r"
r=$("$SXK" do 'smokeX' 2>&1);                   check "state persists"   "7" "$r"
r=$("$SXK" do 'Range[3]
% + 10' 2>&1);                                  check "% within cell"    "{11, 12, 13}" "$r"

# try: fast path returns value + timing
r=$("$SXK" try 60 '1 + 1' 2>&1);                check "try fast value"   "2" "$r"
check "try fast timing" "[sxk try]" "$r"
# try: timeout path -- kernel and state must survive
r=$("$SXK" try 2 'Pause[30]; 99' 2>&1);         check "try timeout"      "TIMED OUT after 2 s" "$r"
r=$("$SXK" do 'smokeX' 2>&1);                   check "state after timeout" "7" "$r"

# user-note channel
"$SXK" tell "wrong substrate, use abstract functions" >/dev/null 2>&1
r=$("$SXK" status 2>&1);                        check "tell surfaces in status" "USER NOTE" "$r"
check "tell content" "abstract functions" "$r"
r=$("$SXK" do '3+3' 2>&1);                      check "tell surfaces in do" "USER NOTE" "$r"
r=$("$SXK" ack 2>&1);                           check "ack" "acknowledged" "$r"
r=$("$SXK" status 2>&1)
case "$r" in *"USER NOTE"*) bad "note cleared after ack" ;; *) ok "note cleared after ack" ;; esac

# log: submissions present, plumbing absent
r=$("$SXK" log 2>&1)
check "log has code" "smokeX = 7" "$r"
case "$r" in *__sxk_t*) bad "log has no tokens" ;; *) ok "log has no tokens" ;; esac

# config: precedence chain, refusals, note-style surfacing, enforcement
r=$("$SXK" config 2>&1);                        check "config lists levers" "TRY_SECONDS" "$r"
check "config provenance kit.conf" "(kit.conf)" "$r"
r=$(SXK_STALL_MINUTES=9 "$SXK" config 2>&1 | grep STALL_MINUTES); check "env override wins" "9" "$r"
check "env provenance shown" "env SXK_STALL_MINUTES" "$r"
r=$("$SXK" set FOO 1 2>&1);                     check "unknown key refused" "unknown config key" "$r"
r=$("$SXK" set TRY_SECONDS abc 2>&1);           check "non-integer refused" "positive integers" "$r"
r=$("$SXK" set TRY_SECONDS 2 2>&1);             check "set changes value" "TRY_SECONDS 120 -> 2" "$r"
r=$(SXK_TRY_SECONDS=50 "$SXK" config 2>&1 | grep TRY_SECONDS); check "session beats env" "(session, sxk set)" "$r"
r=$("$SXK" status 2>&1);                        check "set surfaces as note" "CONFIG: TRY_SECONDS" "$r"
"$SXK" ack >/dev/null 2>&1
r=$("$SXK" try 'Pause[30]; 99' 2>&1);           check "try enforces session TRY_SECONDS" "TIMED OUT after 2 s" "$r"

# compute channel + WorkAs (the Kernel-side agent verbs, installed paclet)
r=$("$SXK" chan on 2>&1);                       check "chan refuses before load" "NOT loaded" "$r"
"$SXK" do 'Needs["VasilDimitrov`SolutionsX`"]' >/dev/null 2>&1
r=$("$SXK" chan on 2>&1);                       check "chan on after load" "compute channel -> " "$r"
"$SXK" do 'VasilDimitrov`SolutionsX`computeEmit["smokepulse", 1, 2]' >/dev/null 2>&1
r=$(tail -1 "$SXK_DIR/compute.log" 2>/dev/null); check "channel line lands" "smokepulse" "$r"
r=$("$SXK" do 'WorkAs["Smoke-bot"]; {$Alias, $Curator}' 2>&1); check "WorkAs switches, curator off" '{"Smoke-bot", False}' "$r"
r=$("$SXK" do 'WorkAs["NotABot"]' 2>&1);        check "WorkAs refuses non-bot" "agent identities" "$r"
r=$("$SXK" do '$Alias' 2>&1);                   check "alias survives refusal" "Smoke-bot" "$r"
r=$("$SXK" chan off 2>&1);                      check "chan off" "compute channel -> None" "$r"

# bg + watch: heartbeats stream, watch ends at idle
"$SXK" bg 'Do[Print["[hb] ", i]; Pause[1], {i, 3}]; Print["[hb] done"]' >/dev/null 2>&1
r=$("$SXK" watch 1 30 2>&1);                    check "watch sees heartbeats" "[hb]" "$r"
check "watch reaches idle" "idle" "$r"

# honest status after an external kernel kill; again recovers via prefix
echo 'smokeY = 42;' > "$SXK_DIR/prefix.wl"
kpid=$(cat "$SXK_DIR/kpid")
kill -9 "$kpid" 2>/dev/null; sleep 1
r=$("$SXK" status 2>&1);                        check "status honest on dead kernel" "KERNEL IS DEAD" "$r"
r=$("$SXK" do '1' 2>&1);                        check "do refuses dead kernel" "KERNEL IS DEAD" "$r"
r=$("$SXK" again 2>&1);                         check "again restarts" "kernel $K up" "$r"
check "again replays prefix" "replaying prefix" "$r"
r=$("$SXK" do 'smokeY' 2>&1);                   check "prefix state present" "42" "$r"
r=$("$SXK" do 'smokeX' 2>&1)
case "$r" in *7*) bad "old state gone after again" ;; *) ok "old state gone after again" ;; esac

# scoped stop: our processes gone, nothing else touched
"$SXK" stop >/dev/null 2>&1
r=$(ps ax -o command= | grep -F "$SXK_DIR/cmds.wl" | grep -v grep || true)
[ -z "$r" ] && ok "stop leaves no session processes" || bad "stop leaves no session processes"

rm -rf "$SXK_DIR"
echo "== $pass ok, $fail FAIL =="
[ "$fail" -eq 0 ]
