#ifndef DENC_H
#define DENC_H

#include <stddef.h>

// Functions documented as returning an error code all follow this convention:
//   0                     Success
//   ERR_BUFFER_TOO_SMALL  The output buffer must grow; the call can be retried
//   Any positive value    Failure, either an errno or a Windows error code
#define ERR_BUFFER_TOO_SMALL (-1)

// Returns an error code
int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz);

// True or false (nonzero or zero)
int c_is_softlink (const char *path);

// Returns the length of the information stored in a softlink, or a negative
// error code on failure
int c_link_len (const char *path);

// Returns an error code. Fills buf with the target of the softlink at path
int c_link_target (const char *path, char *buf, size_t bufsiz);

// Returns an error code
int c_delete_link (const char *path);

// Returns an error code. is_dir, zero or nonzero, says whether the link is for
// a file or a directory (required on Windows)
int c_create_link (const char *target, const char *name, int is_dir);

#endif
