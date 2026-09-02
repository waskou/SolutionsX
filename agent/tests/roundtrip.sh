#!/bin/bash
# roundtrip.sh -- the reader/author guard: nb -> txt -> nb -> txt' must be
# the identity on the TYPED layer. Comparison is modulo the FE's
# indentation normalization (reparsing normalizes leading whitespace and
# line-wrap continuations -- verified 2026-08-26); everything else must be
# byte-identical.
#
# Usage: tests/roundtrip.sh <notebook.nb> [wl-launcher]
# wl-launcher defaults to wolframscript; pass the path to a `wl` wrapper
# to pick a kernel.
set -uo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
NB="${1:?usage: roundtrip.sh <notebook.nb> [wl-launcher]}"
WL="${2:-wolframscript}"
WORK=$(mktemp -d /tmp/roundtrip-XXXXXX)
trap 'rm -rf "$WORK"' EXIT

run_wl() {  # wolframscript needs -file; a wl wrapper takes the script bare.
            # $WL is deliberately UNQUOTED so a launcher with an argument
            # ("bin/wl 13") word-splits into command + kernel selector.
    case "$WL" in
        *wolframscript) $WL -file "$@" ;;
        *) $WL "$@" ;;
    esac
}

die() { echo "roundtrip: $1" >&2; exit 1; }

# a produced-nothing run must FAIL, not compare two empty files and pass
require_txt() { [ -s "$1" ] || die "FAILED -- $2 produced no output ($1 missing or empty). Launcher: '$WL'"; }

cp "$NB" "$WORK/original.nb"
run_wl "$HERE/nb2txt.wls" "$WORK/original.nb"            >/dev/null
require_txt "$WORK/original.txt" "nb2txt on the original"
run_wl "$HERE/nb-author.wls" "$WORK/original.txt" "$WORK/rebuilt.nb" >/dev/null
[ -s "$WORK/rebuilt.nb" ] || die "FAILED -- nb-author produced no notebook. Launcher: '$WL'"
run_wl "$HERE/nb2txt.wls" "$WORK/rebuilt.nb"             >/dev/null
require_txt "$WORK/rebuilt.txt" "nb2txt on the rebuilt notebook"

CELLS=$(grep -c '^<input' "$WORK/original.txt" || true)
[ "${CELLS:-0}" -gt 0 ] || die "FAILED -- the transcript has zero input cells; nothing was compared"

normalize() {  # join backslash line-wraps, strip leading whitespace, and
               # drop CellLabels (In[n]:= is session HISTORY, not typed
               # content -- a fresh notebook rightly has none)
    sed -e ':a' -e '/\\$/N; s/\\\n//; ta' "$1" \
      | sed -e 's/^[[:space:]]*//' -e 's/^<input [^>]*>/<input>/'
}

normalize "$WORK/original.txt" > "$WORK/a"
normalize "$WORK/rebuilt.txt"  > "$WORK/b"

if diff "$WORK/a" "$WORK/b" > "$WORK/diff"; then
    echo "roundtrip: IDENTITY on the typed layer ($CELLS input cells) -- $NB"
    exit 0
else
    echo "roundtrip: FAILED for $NB -- typed layer changed:"
    cat "$WORK/diff"
    exit 1
fi
