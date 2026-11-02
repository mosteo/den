// All platforms but Windows. Both native sources are always compiled, and the
// one that does not apply to the target is left empty.

#if !defined (_WIN32)

#include "denc.h"

#include <dirent.h>
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

int c_full_path (const char *inputPath, char *fullPath, size_t bufsiz)
{
    (void) inputPath;
    (void) fullPath;
    (void) bufsiz;

    return ENOTSUP;
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

long long c_file_size (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return -last_error ();

    return sb.st_size;
}

int c_getcwd (char *buf, size_t bufsiz)
{
    if (getcwd (buf, bufsiz) != NULL)
        return 0;

    return errno == ERANGE ? ERR_BUFFER_TOO_SMALL : last_error ();
}

int c_mkdir (const char *path)
{
    return mkdir (path, 0777) == 0 ? 0 : last_error ();
}

int c_rmdir (const char *path)
{
    return rmdir (path) == 0 ? 0 : last_error ();
}

int c_unlink (const char *path)
{
    return unlink (path) == 0 ? 0 : last_error ();
}

void *c_dir_open (const char *path, int *error)
{
    DIR *dir = opendir (path);

    if (dir == NULL)
        *error = last_error ();

    return dir;
}

int c_dir_read (void *dir, char *buf, size_t bufsiz)
{
    struct dirent *entry;

    errno = 0; // The only way of telling the end from an error
    entry = readdir ((DIR *) dir);

    if (entry == NULL) {
        if (errno != 0)
            return last_error ();

        buf[0] = '\0';
        return 0;
    }

    if (strlen (entry->d_name) >= bufsiz)
        return ERR_BUFFER_TOO_SMALL;

    strcpy (buf, entry->d_name);
    return 0;
}

void c_dir_close (void *dir)
{
    closedir ((DIR *) dir);
}

int c_copy_file (const char *source, const char *target)
{
    (void) source;
    (void) target;

    return ENOTSUP;
}

int c_copy_attributes (const char *source, const char *target,
                       int permissions, int timestamps)
{
    (void) source;
    (void) target;
    (void) permissions;
    (void) timestamps;

    return ENOTSUP;
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

int c_copy_link (const char *source, const char *name)
{
    size_t size = 1024;

    for (;;) {
        char *target = malloc (size);
        int   result;

        if (target == NULL)
            return ENOMEM;

        result = c_link_target (source, target, size);

        if (result == 0 && symlink (target, name) != 0)
            result = last_error ();

        free (target);

        if (result != ERR_BUFFER_TOO_SMALL)
            return result;

        size *= 2;
    }
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
