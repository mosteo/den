// Windows only. Both native sources are always compiled, and the one that does
// not apply to the target is left empty.

#if defined (_WIN32)

#include "denc.h"

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
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

// Paths are WTF-8 on the Ada side: UTF-8, with the addition of the unpaired
// surrogates that NTFS allows in names and that UTF-8 cannot represent. This
// way any name round-trips, which the Windows conversion functions don't do.

// Decodes the character at in, and advances past it. Returns -1 if malformed.
static int decode_character (const unsigned char **in)
{
    const unsigned char *s = *in;
    int continuation;
    int minimum;
    int result;

    if (s[0] < 0x80) {
        result = s[0];        continuation = 0; minimum = 0;
    } else if ((s[0] & 0xE0) == 0xC0) {
        result = s[0] & 0x1F; continuation = 1; minimum = 0x80;
    } else if ((s[0] & 0xF0) == 0xE0) {
        result = s[0] & 0x0F; continuation = 2; minimum = 0x800;
    } else if ((s[0] & 0xF8) == 0xF0) {
        result = s[0] & 0x07; continuation = 3; minimum = 0x10000;
    } else {
        return -1;
    }

    for (int i = 1; i <= continuation; i++) {
        if ((s[i] & 0xC0) != 0x80) // This also stops at the terminator
            return -1;
        result = (result << 6) | (s[i] & 0x3F);
    }

    // Overlong forms could otherwise smuggle in a separator
    if (result < minimum || result > 0x10FFFF)
        return -1;

    *in = s + continuation + 1;
    return result;
}

// Returns the wide version of path, to be freed by the caller, or NULL if it
// cannot be converted, with the reason in error
static WCHAR *to_wide (const char *path, int *error)
{
    // No sequence of bytes takes less bytes than the wide characters it needs
    WCHAR *result = malloc ((strlen (path) + 1) * sizeof (WCHAR));
    WCHAR *out    = result;
    const unsigned char *in = (const unsigned char *) path;

    if (result == NULL) {
        *error = ERROR_NOT_ENOUGH_MEMORY;
        return NULL;
    }

    while (*in != 0) {
        int character = decode_character (&in);

        if (character < 0) {
            free (result);
            *error = ERROR_NO_UNICODE_TRANSLATION;
            return NULL;
        }

        if (character >= 0x10000) {
            character -= 0x10000;
            *out++ = (WCHAR) (0xD800 | (character >> 10));
            *out++ = (WCHAR) (0xDC00 | (character & 0x3FF));
        } else {
            *out++ = (WCHAR) character;
        }
    }

    *out = 0;
    return result;
}

// Stores the terminated narrow version of the length characters at in. Returns
// an error code.
static int from_wide (const WCHAR *in, size_t length, char *out, size_t bufsiz)
{
    size_t used = 0;

    for (size_t i = 0; i < length; i++) {
        unsigned int character = in[i];
        size_t       size;

        if (character >= 0xD800 && character <= 0xDBFF && i + 1 < length
            && in[i + 1] >= 0xDC00 && in[i + 1] <= 0xDFFF)
        {
            character = 0x10000 + ((character - 0xD800) << 10)
                + (in[i + 1] - 0xDC00);
            i++;
        }

        size = character < 0x80 ? 1 : character < 0x800 ? 2
            : character < 0x10000 ? 3 : 4;

        if (used + size >= bufsiz)
            return ERR_BUFFER_TOO_SMALL;

        switch (size) {
        case 1:
            out[used] = (char) character;
            break;
        case 2:
            out[used]     = (char) (0xC0 | (character >> 6));
            out[used + 1] = (char) (0x80 | (character & 0x3F));
            break;
        case 3:
            out[used]     = (char) (0xE0 | (character >> 12));
            out[used + 1] = (char) (0x80 | ((character >> 6) & 0x3F));
            out[used + 2] = (char) (0x80 | (character & 0x3F));
            break;
        default:
            out[used]     = (char) (0xF0 | (character >> 18));
            out[used + 1] = (char) (0x80 | ((character >> 12) & 0x3F));
            out[used + 2] = (char) (0x80 | ((character >> 6) & 0x3F));
            out[used + 3] = (char) (0x80 | (character & 0x3F));
            break;
        }

        used += size;
    }

    if (used >= bufsiz)
        return ERR_BUFFER_TOO_SMALL;

    out[used] = '\0';
    return 0;
}

// Returns the regular form, when there is one, of a path in the prefixed forms
// that the OS uses to report (\\?\) and to store (\??\) paths. The given buffer
// may be modified, and the returned path is not terminated.
static const WCHAR *regular_path (WCHAR *path, size_t *length)
{
    if (*length < 4 || path[0] != L'\\' || (path[1] != L'\\' && path[1] != L'?')
        || path[2] != L'?' || path[3] != L'\\')
    {
        return path; // Not prefixed
    }

    // \\?\UNC\server\share --> \\server\share
    if (*length >= 8 && _wcsnicmp (path + 4, L"UNC\\", 4) == 0) {
        path[6] = L'\\';
        *length -= 6;
        return path + 6;
    }

    // \\?\C:\dir --> C:\dir
    if (*length >= 6 && path[5] == L':'
        && ((path[4] >= L'a' && path[4] <= L'z')
            || (path[4] >= L'A' && path[4] <= L'Z')))
    {
        *length -= 4;
        return path + 4;
    }

    // There is no other form for the likes of \\?\Volume{guid}\dir
    path[1] = L'\\';
    return path;
}

// Opens path only to query it, which requires no rights on it and doesn't clash
// with anyone else using it. Returns an error code.
static int open_for_query (const char *path, DWORD flags, HANDLE *file)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);

    if (wide == NULL)
        return error;

    *file = CreateFileW
        (wide, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
         NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | flags, NULL);

    if (*file == INVALID_HANDLE_VALUE)
        error = last_error ();

    free (wide);
    return error;
}

// Reads the reparse point at path, without following it. Returns an error code.
static int read_reparse_point (const char *path, reparse_buffer *buffer)
{
    DWORD  returned;
    HANDLE file;
    int    error = open_for_query (path, FILE_FLAG_OPEN_REPARSE_POINT, &file);

    if (error != 0)
        return error;

    if (!DeviceIoControl (file, FSCTL_GET_REPARSE_POINT, NULL, 0,
                          buffer, sizeof (*buffer), &returned, NULL))
        error = last_error ();

    CloseHandle (file);
    return error;
}

// Finds the target stored in a symlink or junction, which is not terminated.
// Returns an error code.
static int stored_target (reparse_buffer *buffer,
                          const WCHAR   **target,
                          size_t         *length)
{
    WCHAR *names;
    USHORT print_offset;
    USHORT print_length;
    USHORT substitute_offset;
    USHORT substitute_length;

    switch (buffer->data.ReparseTag) {
    case IO_REPARSE_TAG_SYMLINK:
        names             = buffer->data.SymbolicLinkReparseBuffer.PathBuffer;
        print_offset      = buffer->data.SymbolicLinkReparseBuffer.PrintNameOffset;
        print_length      = buffer->data.SymbolicLinkReparseBuffer.PrintNameLength;
        substitute_offset = buffer->data.SymbolicLinkReparseBuffer.SubstituteNameOffset;
        substitute_length = buffer->data.SymbolicLinkReparseBuffer.SubstituteNameLength;
        break;
    case IO_REPARSE_TAG_MOUNT_POINT:
        names             = buffer->data.MountPointReparseBuffer.PathBuffer;
        print_offset      = buffer->data.MountPointReparseBuffer.PrintNameOffset;
        print_length      = buffer->data.MountPointReparseBuffer.PrintNameLength;
        substitute_offset = buffer->data.MountPointReparseBuffer.SubstituteNameOffset;
        substitute_length = buffer->data.MountPointReparseBuffer.SubstituteNameLength;
        break;
    default:
        return ERROR_NOT_A_REPARSE_POINT;
    }

    if (print_length > 0) {
        *target = names + print_offset / sizeof (WCHAR);
        *length = print_length / sizeof (WCHAR);
    } else {
        // The print name is optional, and not all tools store it
        *length = substitute_length / sizeof (WCHAR);
        *target = regular_path (names + substitute_offset / sizeof (WCHAR),
                                length);
    }

    return *length > 0 ? 0 : ERROR_INVALID_REPARSE_DATA;
}

int c_canonical (const char *inputPath, char *fullPath, size_t bufsiz)
{
    HANDLE file;
    DWORD  needed;
    DWORD  length;
    WCHAR *path;
    int    result = open_for_query (inputPath, 0, &file);

    if (result != 0)
        return result;

    // The size needed including terminator, or 0 for a broken link or the like
    // NOTE: recursive links resolve to themselves in Windows!!
    needed = GetFinalPathNameByHandleW (file, NULL, 0, VOLUME_NAME_DOS);
    path   = needed == 0 ? NULL : malloc (needed * sizeof (WCHAR));

    if (needed == 0) {
        result = last_error ();
    } else if (path == NULL) {
        result = ERROR_NOT_ENOUGH_MEMORY;
    } else {
        // The length excluding terminator, unless it has grown in the meantime
        length = GetFinalPathNameByHandleW
            (file, path, needed, VOLUME_NAME_DOS);

        if (length == 0) {
            result = last_error ();
        } else if (length >= needed) {
            result = ERR_BUFFER_TOO_SMALL;
        } else {
            size_t       regular_length = length;
            const WCHAR *regular = regular_path (path, &regular_length);

            result = from_wide (regular, regular_length, fullPath, bufsiz);
        }
    }

    free (path);
    CloseHandle (file);
    return result;
}

int c_is_softlink (const char *path)
{
    reparse_buffer buffer;

    return read_reparse_point (path, &buffer) == 0
        && buffer.data.ReparseTag == IO_REPARSE_TAG_SYMLINK;
}

int c_link_target (const char *path, char *buf, size_t bufsiz)
{
    reparse_buffer buffer;
    const WCHAR   *target;
    size_t         length;
    int            error = read_reparse_point (path, &buffer);

    if (error != 0)
        return error;

    error = stored_target (&buffer, &target, &length);

    if (error != 0)
        return error;

    return from_wide (target, length, buf, bufsiz);
}

int c_delete_link (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);
    DWORD  attributes;

    if (wide == NULL)
        return error;

    attributes = GetFileAttributesW (wide);

    if (attributes == INVALID_FILE_ATTRIBUTES) {
        error = last_error ();
    } else if (!(attributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
        error = ERROR_NOT_A_REPARSE_POINT;
    } else if (!((attributes & FILE_ATTRIBUTE_DIRECTORY)
                 ? RemoveDirectoryW (wide) // Directory links are directories
                 : DeleteFileW (wide)))
    {
        error = last_error ();
    }

    free (wide);
    return error;
}

int c_create_link (const char *target, const char *name, int is_dir)
{
    int    error       = 0;
    DWORD  flags       = is_dir ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0;
    WCHAR *wide_target = to_wide (target, &error);
    WCHAR *wide_name   = wide_target == NULL ? NULL : to_wide (name, &error);

    if (wide_name != NULL) {
        // Targets are stored as given, and the OS does not take a stored '/'
        // as a separator, so such a link would be broken
        for (WCHAR *c = wide_target; *c != 0; c++) {
            if (*c == L'/')
                *c = L'\\';
        }

        // Allows unprivileged users to create links in Developer Mode, but
        // systems that predate this flag reject it
        if (!CreateSymbolicLinkW
                (wide_name, wide_target,
                 flags | SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE)
            && !(GetLastError () == ERROR_INVALID_PARAMETER
                 && CreateSymbolicLinkW (wide_name, wide_target, flags)))
        {
            error = last_error ();
        }
    }

    free (wide_target);
    free (wide_name);
    return error;
}

#else

// ISO C forbids an empty translation unit
typedef int denc_windows_not_applicable;

#endif
