// All platforms but Windows. Both native sources are always compiled, and the
// one that does not apply to the target is left empty.

#if !defined (_WIN32)

#include "denc.h"

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifndef PATH_MAX
    #define PATH_MAX 8192
#endif

// The error code for a failed call. On some platforms errno values are negative
static int last_error (void)
{
    return errno == 0 ? EIO : abs (errno);
}

int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz)
{
    char *resolved = realpath (inputPath, NULL);

    if (resolved == NULL)
        return last_error ();

    if (strlen (resolved) >= bufsiz) {
        free (resolved);
        return ERR_BUFFER_TOO_SMALL;
    }

    strcpy (fullPath, resolved);
    free (resolved);

    return 0;
}

int c_is_softlink (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return 0;

    return S_ISLNK (sb.st_mode);
}

int c_link_len (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return -last_error ();

    // Some magic symlinks under (for example) /proc and /sys report 'st_size'
    // as zero. In that case, take PATH_MAX as a "good enough" estimate.

    return (sb.st_size == 0) ? PATH_MAX : (int) sb.st_size;
}

int c_link_target (const char *path, char *buf, size_t bufsiz)
{
    ssize_t nbytes = readlink (path, buf, bufsiz);

    if (nbytes == -1)
        return last_error ();

    // If the return value was equal to the buffer size, then the link target
    // was larger than expected (perhaps because the target was changed
    // between the call to lstat() and the call to readlink()).

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
