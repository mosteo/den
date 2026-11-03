#!/bin/bash
# Fetches a mingw-w64 GNAT cross compiler and Wine into wine/tools, without root
# and without installing anything in the system. Needs apt-get, so Debian or
# Ubuntu. Run it again after deleting wine/tools to start over.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
TOOLS=$HERE/tools

PACKAGES=(
    gnat-mingw-w64-x86-64-win32
    gcc-mingw-w64-x86-64-win32
    gcc-mingw-w64-x86-64-win32-runtime
    gcc-mingw-w64-base
    binutils-mingw-w64-x86-64
    mingw-w64-common
    mingw-w64-x86-64-dev
    wine
    wine64
    libwine
    libz-mingw-w64
)

mkdir -p "$TOOLS/debs" "$TOOLS/root"
cd "$TOOLS/debs"
apt-get download "${PACKAGES[@]}"
for deb in *.deb; do
    dpkg -x "$deb" "$TOOLS/root"
done

# The packages leave the unsuffixed names to update-alternatives, which needs
# root, so make them here
cd "$TOOLS/root/usr/bin"
for tool in x86_64-w64-mingw32-*-win32; do
    ln -sf "$tool" "${tool%-win32}"
done

# Wine runs the wineserver next to it, which is a script that looks for an
# installed Wine, so use the real one instead
ln -sf wineserver64 "$TOOLS/root/usr/lib/wine/wineserver"

# Wine looks for zlib in the system's mingw folder, so put it with its own DLLs
cp "$TOOLS/root/usr/x86_64-w64-mingw32/lib/zlib1.dll" \
   "$TOOLS/root/usr/lib/x86_64-linux-gnu/wine/x86_64-windows/"

"$TOOLS/root/usr/bin/x86_64-w64-mingw32-gcc" -c -O2 -Wall -Wextra \
    "$HERE/gnat_wide_fix.c" -o "$TOOLS/gnat_wide_fix.o"

echo "Tools ready in $TOOLS"
