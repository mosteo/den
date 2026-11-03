// Debian's mingw-w64 GNAT runtime is built as a cross runtime, which makes
// these runtime functions use the ANSI Windows API. Native GNATs for Windows,
// as used in CI, use the wide API with UTF-8 strings, as these do. Without
// them, tests that make files with non-ASCII names through the Ada library fail
// under Wine for reasons unrelated to Den. Linked into the test mains only.

#include <direct.h>
#include <io.h>
#include <stdio.h>
#include <windows.h>

#define MAX_CHARS 4096 // Far over any path the tests give to these

#define WIDE(name, path)        \
    wchar_t name[MAX_CHARS];    \
    MultiByteToWideChar (CP_UTF8, 0, path, -1, name, MAX_CHARS)

FILE *__gnat_fopen (char *path, char *mode, int encoding)
{
    WIDE (wide_path, path);
    WIDE (wide_mode, mode);
    (void) encoding;
    return _wfopen (wide_path, wide_mode);
}

int __gnat_unlink (char *path)
{
    WIDE (wide, path);
    return _wunlink (wide);
}

int __gnat_rmdir (char *path)
{
    WIDE (wide, path);
    return _wrmdir (wide);
}

int __gnat_chdir (char *path)
{
    WIDE (wide, path);
    return _wchdir (wide);
}

int __gnat_rename (char *from, char *to)
{
    WIDE (wide_from, from);
    WIDE (wide_to, to);
    return _wrename (wide_from, wide_to);
}
