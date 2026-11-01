// Windows only. Both native sources are always compiled, and the one that does
// not apply to the target is left empty.

#if defined (_WIN32)

#include "denc.h"

#include <limits.h>
#include <string.h>
#include <windows.h>
#include <winioctl.h>

// The contents of a reparse point, which are not declared by the user-mode
// Windows headers
typedef struct {
    ULONG  ReparseTag;
    USHORT ReparseDataLength;
    USHORT Reserved;
    union {
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            ULONG  Flags;
            WCHAR  PathBuffer[1];
        } SymbolicLinkReparseBuffer;
        struct {
            USHORT SubstituteNameOffset;
            USHORT SubstituteNameLength;
            USHORT PrintNameOffset;
            USHORT PrintNameLength;
            WCHAR  PathBuffer[1];
        } MountPointReparseBuffer;
        struct {
            UCHAR DataBuffer[1];
        } GenericReparseBuffer;
    };
} reparse_data;

// Storage for the largest reparse point, correctly aligned for reparse_data
typedef union {
    reparse_data data;
    BYTE         bytes[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
} reparse_buffer;

// The error code for a failed call. It can be neither 0, which means success,
// nor negative, which ERR_BUFFER_TOO_SMALL is
static int last_error (void)
{
    DWORD error = GetLastError ();

    return (error == 0 || error > INT_MAX) ? ERROR_GEN_FAILURE : (int) error;
}

// Reads the reparse point at path, without following it. Returns an error code.
static int read_reparse_point (const char *path, reparse_buffer *buffer)
{
    DWORD  returned;
    int    result = 0;
    HANDLE file   = CreateFileA
        (path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
         NULL, OPEN_EXISTING,
         FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (file == INVALID_HANDLE_VALUE)
        return last_error ();

    if (!DeviceIoControl (file, FSCTL_GET_REPARSE_POINT, NULL, 0,
                          buffer, sizeof (*buffer), &returned, NULL))
        result = last_error ();

    CloseHandle (file);
    return result;
}

int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz)
{
    DWORD  length;
    int    result = 0;
    HANDLE file   = CreateFileA
        (inputPath, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
         FILE_FLAG_BACKUP_SEMANTICS, NULL);

    if (file == INVALID_HANDLE_VALUE)
        return last_error ();

    // Returns the length used, the buffer needed if too small, or 0 for error
    // NOTE: recursive links resolve to themselves in Windows!!
    length = GetFinalPathNameByHandleA
        (file, fullPath, (DWORD) bufsiz, VOLUME_NAME_DOS);

    if (length == 0)
        result = last_error (); // Link is broken or some other error
    else if (length >= bufsiz)
        result = ERR_BUFFER_TOO_SMALL;
    else if (strncmp (fullPath, "\\\\?\\", 4) == 0)
        memmove (fullPath, fullPath + 4, length - 4 + 1); // Remove the prefix

    CloseHandle (file);
    return result;
}

int c_is_softlink (const char *path)
{
    reparse_buffer buffer;

    return read_reparse_point (path, &buffer) == 0
        && buffer.data.ReparseTag == IO_REPARSE_TAG_SYMLINK;
}

int c_link_len (const char *path)
{
    reparse_buffer buffer;
    int            error = read_reparse_point (path, &buffer);

    if (error != 0)
        return -error;

    if (buffer.data.ReparseTag != IO_REPARSE_TAG_SYMLINK)
        return -ERROR_NOT_A_REPARSE_POINT;

    return buffer.data.SymbolicLinkReparseBuffer.PrintNameLength / sizeof (WCHAR);
}

int c_link_target (const char *path, char *buf, size_t bufsiz)
{
    reparse_buffer buffer;
    int            error = read_reparse_point (path, &buffer);
    PWCHAR         target;
    int            target_length;
    int            needed;

    if (error != 0)
        return error;

    if (buffer.data.ReparseTag != IO_REPARSE_TAG_SYMLINK)
        return ERROR_NOT_A_REPARSE_POINT;

    target = buffer.data.SymbolicLinkReparseBuffer.PathBuffer
        + buffer.data.SymbolicLinkReparseBuffer.PrintNameOffset / sizeof (WCHAR);
    target_length =
        buffer.data.SymbolicLinkReparseBuffer.PrintNameLength / sizeof (WCHAR);

    needed = WideCharToMultiByte
        (CP_UTF8, 0, target, target_length, NULL, 0, NULL, NULL);

    if (needed == 0)
        return last_error ();

    if ((size_t) needed >= bufsiz)
        return ERR_BUFFER_TOO_SMALL;

    if (WideCharToMultiByte (CP_UTF8, 0, target, target_length,
                             buf, needed, NULL, NULL) == 0)
        return last_error ();

    buf[needed] = '\0';
    return 0;
}

int c_delete_link (const char *path)
{
    DWORD attributes = GetFileAttributesA (path);
    BOOL  deleted;

    if (attributes == INVALID_FILE_ATTRIBUTES)
        return last_error ();

    if (!(attributes & FILE_ATTRIBUTE_REPARSE_POINT))
        return ERROR_NOT_A_REPARSE_POINT;

    // Directory symlinks are removed as directories, file symlinks as files
    deleted = (attributes & FILE_ATTRIBUTE_DIRECTORY)
        ? RemoveDirectoryA (path)
        : DeleteFileA (path);

    return deleted ? 0 : last_error ();
}

int c_create_link (const char *target, const char *name, int is_dir)
{
    // Allows unprivileged users to create symlinks in Developer Mode
    DWORD flags = SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;

    if (is_dir)
        flags |= SYMBOLIC_LINK_FLAG_DIRECTORY;

    return CreateSymbolicLinkA (name, target, flags) ? 0 : last_error ();
}

#else

// ISO C forbids an empty translation unit
typedef int denc_windows_not_applicable;

#endif
