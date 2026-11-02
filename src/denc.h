#ifndef DENC_H
#define DENC_H

#include <stddef.h>

// Strings are the bytes stored by the filesystem on Unix-like systems, and UTF-8
// on Windows, regardless of the code page (see denc_windows.c for the details).

// Functions documented as returning an error code all follow this convention:
//   0                     Success
//   ERR_BUFFER_TOO_SMALL  The output buffer must grow; the call can be retried
//   Any positive value    Failure, either an errno or a Windows error code
#define ERR_BUFFER_TOO_SMALL (-1)

// Returns an error code
int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz);

// Returns an error code. Fills fullPath with the absolute form that Windows gives
// to inputPath. It doesn't apply elsewhere, where paths are either absolute or
// relative to the current directory, so it just fails.
int c_full_path (const char *inputPath, char *fullPath, size_t bufsiz);

// What is at path, without following it. Failures of any kind mean there is
// nothing reachable there. The values match those of Den.Kinds.
enum { DEN_NOTHING, DEN_DIRECTORY, DEN_FILE, DEN_SOFTLINK, DEN_SPECIAL };
int c_kind (const char *path);

// Returns the size of the file at path, or a negated error code
long long c_file_size (const char *path);

// Returns an error code. Fills buf with the current directory
int c_getcwd (char *buf, size_t bufsiz);

// Returns an error code
int c_mkdir (const char *path);

// Returns an error code. Read-only directories can be removed on all platforms.
int c_rmdir (const char *path);

// Returns an error code. Read-only files can be deleted on all platforms.
int c_unlink (const char *path);

// Directory listing. Open returns NULL on error, with its code stored in error.
// Read returns an error code, and fills buf with the next name, which is empty
// when there are no more. All entries are returned, including "." and "..".
void *c_dir_open (const char *path, int *error);
int c_dir_read (void *dir, char *buf, size_t bufsiz);
void c_dir_close (void *dir);

// Return an error code. These only apply to Windows, where the runtime cannot
// deal with long paths, and elsewhere they just fail. The copy doesn't preserve
// attributes, which is what the other subprogram does, as requested.
int c_copy_file (const char *source, const char *target);
int c_copy_attributes (const char *source, const char *target,
                       int permissions, int timestamps);

// Returns an error code. Fills buf with the target of the softlink at path
int c_link_target (const char *path, char *buf, size_t bufsiz);

// Returns an error code
int c_delete_link (const char *path);

// Returns an error code. is_dir, zero or nonzero, says whether the link is for
// a file or a directory (required on Windows)
int c_create_link (const char *target, const char *name, int is_dir);

#endif
