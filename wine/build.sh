#!/bin/bash
# Cross-builds the test mains for Windows into wine/build, all of them or those
# named, as in: wine/build.sh den_tests-kinds den_tests-windows
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(dirname "$HERE")
TOOLS=$HERE/tools
OUT=$HERE/build

# Alire generates the configuration and fetches the dependencies. Every project
# it reports keeps its sources in src/ and config/, and the tests also in common/
cd "$ROOT/tests"
alr -n build --stop-after=generation > /dev/null
IFS=: read -ra PROJECTS <<< "$(alr -n exec -- printenv GPR_PROJECT_PATH | tail -1)"

INCLUDES=()
for project in "${PROJECTS[@]}"; do
    for dir in src config common; do
        if [ -d "$project/$dir" ]; then
            INCLUDES+=("-aI$project/$dir")
        fi
    done
done

export PATH=$TOOLS/root/usr/bin:$PATH
mkdir -p "$OUT"
cd "$OUT"

x86_64-w64-mingw32-gcc -g -Wall -Wextra -c "$ROOT/src/denc_windows.c" \
    -o denc_windows.o

MAINS=("$@")
if [ ${#MAINS[@]} -eq 0 ]; then
    MAINS=($(cd "$ROOT/tests/src" && ls *.adb | sed 's/\.adb$//'))
fi

failed=0
for main in "${MAINS[@]}"; do
    if x86_64-w64-mingw32-gnatmake -q -j0 -gnat2012 -gnata -gnato -gnatW8 -g \
         "${INCLUDES[@]}" "$main" \
         -largs denc_windows.o "$TOOLS/gnat_wide_fix.o" \
                -Wl,--allow-multiple-definition \
         > "$main.build.log" 2>&1
    then
        echo "built  $main"
    else
        echo "FAILED $main, see $OUT/$main.build.log"
        failed=$((failed + 1))
    fi
done

exit $((failed > 0))
