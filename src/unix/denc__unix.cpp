#include "../denc.h"

#include <cstdlib>
#include <cstring>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(__APPLE__)
    #include <sys/syslimits.h>
#elif defined(__unix__) || defined(__unix)
    #include <limits.h>
#else
    #error "Unsupported platform"
#endif

#ifndef PATH_MAX
    #define PATH_MAX 8192
#endif

//  Returns 0 for success, -1 for not enough buffer, >=1 for non-recoverable
//  error
extern "C" int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz)
{
    char *resolved = realpath (inputPath, NULL);

    if (resolved == NULL)
        return abs(errno); // In case errno was negative
    else if (strlen(resolved) >= bufsiz) {
        free(resolved);
        return ERR_BUFFER_TOO_SMALL; // Use the defined constant
    } else {
        strcpy(fullPath, resolved);
        free(resolved);
    }

    return 0;
}

//  True or false (nonzero or zero)
extern "C" int c_is_softlink (const char *path)
{
    struct stat sb;

    if (lstat (path, &sb) == -1)
        return 0;

    return S_ISLNK (sb.st_mode);
}


// Returns the length of the information stored in a softlink, or a negative
// error code on failure
extern "C" int c_link_len (const char *path)
{
  struct stat sb;

  if (lstat (path, &sb) == -1)
    return -1;

  /* Some magic symlinks under (for example) /proc and /sys report 'st_size'
     as zero. In that case, take PATH_MAX as a "good enough" estimate. */

  return (sb.st_size == 0) ? PATH_MAX : sb.st_size;
}

// Returns 0 on success, -1 on not enough buffer, or positive error code on failure
extern "C" int c_link_target (const char *path, char *buf, size_t bufsiz) {
    ssize_t nbytes = readlink(path, buf, bufsiz);
    if (nbytes == -1) {
        return abs(errno); // In case errno was negative
    }

    /* If the return value was equal to the buffer size, then the link target
       was larger than expected (perhaps because the target was changed
       between the call to lstat() and the call to readlink()). */

    if (nbytes == (ssize_t)bufsiz)
        return ERR_BUFFER_TOO_SMALL;

    buf[nbytes] = '\0';
    return 0;
}

extern "C"
int c_delete_link (const char *path)
{
    if (!c_is_softlink (path))
        return -1;

    return unlink(path);
}

extern "C"
int c_create_link (const char *target, const char *name, const bool is_dir)
{
    return symlink(target, name);
}
