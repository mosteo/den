#define ERR_BUFFER_TOO_SMALL (-1)

#include <stddef.h>

//  Returns 0 for success, -1 for not enough buffer, >=1 for non-recoverable error
extern "C" int c_canonical(const char* inputPath, char* fullPath, size_t bufsiz);

//  True or false (nonzero or zero)
extern "C" int c_is_softlink(const char *path);

// Returns the length of the information stored in a softlink, or a negative Windows error code on failure
extern "C" int c_link_len(const char *path);

// Returns 0 on success, -1 on not enough buffer, or positive error code on failure
extern "C" int c_link_target(const char *path, char *buf, size_t bufsiz);

// Returns 0 on success, any other for error
extern "C" int c_delete_link (const char *path);

// Returns 0 on success, any other for error.
// Say if this is for a file or directory symlink (required on Windows).
extern "C" int c_create_link (const char *target, const char *name, const bool is_dir);