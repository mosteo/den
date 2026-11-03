#!/bin/bash
# Runs the test mains built by build.sh under Wine, all of them or those named,
# on a fresh copy of the repository so that failures leave nothing behind. The
# output of each goes to wine/build/<main>.log.
set -uo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(dirname "$HERE")
TOOLS=$HERE/tools
OUT=$HERE/build
RUN=$OUT/run
TIMEOUT=150 # Seconds for each test

export WINEPREFIX=$TOOLS/prefix
export WINEARCH=win64
export WINEDEBUG=-all
export WINEDLLOVERRIDES="mscoree,mshtml=" # Don't ask to install Mono and Gecko
export WINESERVER=$TOOLS/root/usr/lib/wine/wineserver64
WINE=$TOOLS/root/usr/lib/wine/wine64

rsync -a --delete --exclude=.git --exclude=/wine/ \
      --exclude=alire/ --exclude=obj/ --exclude=lib/ --exclude=bin/ \
      "$ROOT/" "$RUN/"
mkdir -p "$RUN/tests/bin" # As made by alr test, and expected by the selftest

MAINS=("$@")
if [ ${#MAINS[@]} -eq 0 ]; then
    MAINS=($(cd "$OUT" && ls *.exe | sed 's/\.exe$//'))
fi

cd "$RUN/tests"
failed=0
for main in "${MAINS[@]}"; do
    timeout "$TIMEOUT" "$WINE" "$OUT/$main.exe" > "$OUT/$main.log" 2>&1
    code=$?
    if [ $code -eq 0 ]; then
        echo "PASS $main"
    else
        failed=$((failed + 1))
        if [ $code -eq 124 ]; then
            echo "FAIL $main: timed out"
        else
            echo "FAIL $main: $(grep -a -m1 raised "$OUT/$main.log" | tr -d '\r')"
        fi
    fi
done

"$WINESERVER" -k
exit $((failed > 0))
