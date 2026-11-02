// All platforms but Windows. Both native sources are always compiled, and the
// one that does not apply to the target is left empty.

#if !defined (_WIN32)

#include "denc.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

// The error code for a failed call. On some platforms errno values are negative
static int last_error (void)
{
    return errno == 0 ? EIO : abs (errno);
}

int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz)
{
    struct stat sb;
    char *resolved = realpath (inputPath, NULL);
    int   result   = 0;

    if (resolved == NULL)
        return last_error ();

    // Some systems don't fail when only the last component is missing
    if (lstat (resolved, &sb) == -1)
        result = last_error ();
    else if (strlen (resolved) >= bufsiz)
        result = ERR_BUFFER_TOO_SMALL;
    else
        strcpy (fullPath, resolved);

    free (resolved);
    return result;
}

int c_kind (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return DEN_NOTHING;

    return S_ISLNK (sb.st_mode) ? DEN_SOFTLINK
        : S_ISDIR (sb.st_mode)  ? DEN_DIRECTORY
        : S_ISREG (sb.st_mode)  ? DEN_FILE
        : DEN_SPECIAL;
}

int c_link_target (const char *path, char *buf, size_t bufsiz)
{
    ssize_t nbytes = readlink (path, buf, bufsiz);

    if (nbytes == -1)
        return last_error ();

    // A result that fills the buffer may have been truncated

    if (nbytes == (ssize_t) bufsiz)
        return ERR_BUFFER_TOO_SMALL;

    buf[nbytes] = '\0';
    return 0;
}

int c_delete_link (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return last_error ();

    if (!S_ISLNK (sb.st_mode))
        return EINVAL;

    return unlink (path) == 0 ? 0 : last_error ();
}

int c_create_link (const char *target, const char *name, int is_dir)
{
    (void) is_dir; // Only meaningful on Windows

    return symlink (target, name) == 0 ? 0 : last_error ();
}

#else

// ISO C forbids an empty translation unit
typedef int denc_unix_not_applicable;

#endif
