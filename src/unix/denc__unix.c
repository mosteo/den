//  The purpose of this file is to be able to use readlink portably

#if defined(__APPLE__)
    #include <sys/syslimits.h>
    #include <sys/stat.h>
    #include <unistd.h>
    #include <errno.h>
#elif defined(__unix__) || defined(__unix)
    #include <errno.h>
    #include <limits.h>
    #include <stdlib.h>
    #include <sys/stat.h>
    #include <unistd.h>
#else
    #error "Unsupported platform"
#endif

#ifndef PATH_MAX
    #define PATH_MAX 8192
#endif

ssize_t link_len(const char *path) {
    struct stat sb;

    if (lstat(path, &sb) == -1)
        return -1;

    /* Some magic symlinks under (for example) /proc and /sys report 'st_size'
       as zero. In that case, take PATH_MAX as a "good enough" estimate. */

    return (sb.st_size == 0) ? PATH_MAX : sb.st_size;
}

int link_target(const char *path, char *buf, ssize_t bufsiz) {
    ssize_t nbytes = readlink(path, buf, bufsiz);
    if (nbytes == -1) {
        return errno;
    }

    /* If the return value was equal to the buffer size, then the link target
       was larger than expected (perhaps because the target was changed
       between the call to lstat() and the call to readlink()). */

    if (nbytes == bufsiz)
        return -1;

    buf[nbytes] = '\0';
    return 0;
}

/*
bool supports_symlinks(const char *path) {
#if defined(_WIN32)
    // Check if the volume supports reparse points (which includes symlinks)
    char root_path[4];
    strncpy(root_path, path, 3);
    root_path[3] = '\0';

    DWORD flags;
    if (GetVolumeInformation(root_path, NULL, 0, NULL, NULL, &flags, NULL, 0)) {
        return (flags & FILE_SUPPORTS_REPARSE_POINTS) != 0;
    }
    return false;
#elif defined(__APPLE__) || defined(__unix__) || defined(__unix)
    // Try to create a temporary symlink
    char temp_file[PATH_MAX];
    char temp_link[PATH_MAX];

    snprintf(temp_file, sizeof(temp_file), "%s/temp_file_XXXXXX", path);
    snprintf(temp_link, sizeof(temp_link), "%s/temp_link_XXXXXX", path);

    int fd = mkstemp(temp_file);
    if (fd == -1) return false;
    close(fd);

    if (symlink(temp_file, temp_link) == 0) {
        unlink(temp_link);
        unlink(temp_file);
        return true;
    } else {
        unlink(temp_file);
        return false;
    }
#else
    return false;
#endif
}
*/
