// include headers for readlink call, only if not on Windows
#ifdef _WIN32
    #define readlink(a, b, c) -1
#else
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

    // buf = malloc(bufsiz);
    // if (buf == NULL) {
    //     perror("malloc");
    //     exit(EXIT_FAILURE);
    // }

    // nbytes = readlink(argv[1], buf, bufsiz);
    // if (nbytes == -1) {
    //     perror("readlink");
    //     exit(EXIT_FAILURE);
    // }

    // printf("'%s' points to '%.*s'\n", argv[1], (int) nbytes, buf);

    // /* If the return value was equal to the buffer size, then the
    //     the link target was larger than expected (perhaps because the
    //     target was changed between the call to lstat() and the call to
    //     readlink()). Warn the user that the returned target may have
