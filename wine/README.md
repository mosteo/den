# Testing the Windows build under Wine

These scripts cross-build the test mains for Windows and run them under Wine,
for a quick check of Windows code on Linux before pushing to CI. Wine is not
Windows, so CI on real Windows remains the reference: see the known failures
below.

## Use

```sh
wine/setup.sh                  # Once: tools into wine/tools (~1.3 GB, no root)
wine/build.sh                  # Build all test mains into wine/build
wine/test.sh                   # Run all of them under Wine
wine/build.sh den_tests-kinds  # Or only some, named as in tests/src
wine/test.sh den_tests-kinds
```

`setup.sh` needs `apt-get`, so Debian or Ubuntu, and was made on Ubuntu 24.04
with GNAT 13 and Wine 9.0. `build.sh` needs `alr`, which it uses to fetch the
dependencies, and it regenerates `config/` as `alr test` does.

The tests run from a fresh copy of the repository in `wine/build/run`, so they
leave nothing in the checkout. Each main's output is in `wine/build/<main>.log`.
The first run is slow because Wine creates its prefix in `wine/tools/prefix`.

## Known failures

Wine falls short of Windows in ways that some tests notice:

| Test | Cause |
|---|---|
| `c_interface`, `relative_links` | Wine cannot create symlinks |
| `selftests` | Wine misses the link in the middle of `cases/links/i/h` |
| `unicode` | Wine rejects names with unpaired surrogates |
| `windows` | Wine cannot canonicalize long paths |
| `example` | Times out under Wine, though it passes on Windows |

Any other failure is worth a look.

## Why `gnat_wide_fix.c`

Debian's mingw-w64 GNAT runtime is built as a cross runtime, so its file
functions use the ANSI Windows API instead of the wide one that native Windows
GNATs use. Tests that make non-ASCII names through the Ada library would fail
for that alone, so the test mains link these replacements.
