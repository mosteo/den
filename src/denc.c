// include headers for readlink call, only if not on Windows
#ifdef _WIN32
    #define readlink(a, b, c) -1
#else
    #include <errno.h>
    #include <limits.h>
    #include <stdlib.h>
    #include <sys/stat.h>
    #include <unistd.h>
#endif

ssize_t link_len(const char *path) {
    ssize_t len;

    struct stat sb;
    ssize_t nbytes, bufsiz;

    if (lstat(path, &sb) == -1)
        return -1;

    /* Some magic symlinks under (for example) /proc and /sys report 'st_size'
        as zero. In that case, take PATH_MAX as a "good enough" estimate. */

    if (sb.st_size == 0)
        return PATH_MAX;
    else
        return sb.st_size;
}

int link_target(const char *path, char *buf, ssize_t bufsiz) {

    int nbytes = readlink(path, buf, bufsiz);
    if (nbytes == -1) {
        return errno;
    }

    /* If the return value was equal to the buffer size, then the link target
        was larger than expected (perhaps because the target was changed
        between the call to lstat() and the call to readlink()). */

    if (nbytes == bufsiz)
        return -1;
    else {
        buf[nbytes] = '\0';
        return 0;
    }

}