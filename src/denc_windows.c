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

// These are missing from the headers of older compilers
#define REPARSE_TAG_AF_UNIX 0x80000023
#define REPARSE_TAG_LX_FIFO 0x80000024
#define REPARSE_TAG_LX_CHR  0x80000025
#define REPARSE_TAG_LX_BLK  0x80000026

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

// Returns the wide version of path exactly as given, to be freed by the caller,
// or NULL if it cannot be converted, with the reason in error
static WCHAR *decode_path (const char *path, int *error)
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

// Most of the API is limited to MAX_PATH characters, and to 248 for creating
// directories, unless paths are given in the verbatim form.
#define LONGEST_REGULAR_PATH 248

// Returns the wide version of path to use with the OS, to be freed by the caller,
// or NULL if it cannot be converted, with the reason in error. Long paths are
// converted to the verbatim form, which requires them to be absolute and to use
// only backslashes. Only the full path says whether it is long.
static WCHAR *to_wide (const char *path, int *error)
{
    WCHAR *wide = decode_path (path, error);
    WCHAR *full;
    WCHAR *result;
    DWORD  needed;

    if (wide == NULL)
        return NULL;

    // These are already in their final form
    if (wcsncmp (wide, L"\\\\?\\", 4) == 0
        || wcsncmp (wide, L"\\\\.\\", 4) == 0)
    {
        return wide;
    }

    // The size needed including terminator, or 0 for error, which the call this
    // path is meant for will report as well
    needed = GetFullPathNameW (wide, 0, NULL, NULL);

    if (needed <= LONGEST_REGULAR_PATH)
        return wide;

    full   = malloc (needed * sizeof (WCHAR));
    result = malloc ((needed + 8) * sizeof (WCHAR)); // Room for the prefix

    if (full == NULL || result == NULL) {
        *error = ERROR_NOT_ENOUGH_MEMORY;
        free (result);
        result = NULL;
    } else if (GetFullPathNameW (wide, needed, full, NULL) == 0) {
        *error = last_error ();
        free (result);
        result = NULL;
    } else if (full[0] == L'\\' && full[1] == L'\\') {
        wcscpy (result, L"\\\\?\\UNC\\");
        wcscat (result, full + 2);
    } else {
        wcscpy (result, L"\\\\?\\");
        wcscat (result, full);
    }

    free (full);
    free (wide);
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
static int open_for_query (const WCHAR *path, DWORD flags, HANDLE *file)
{
    *file = CreateFileW
        (path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
         NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | flags, NULL);

    return *file == INVALID_HANDLE_VALUE ? last_error () : 0;
}

// Reads the reparse point at path, without following it. Returns an error code.
static int read_reparse_point (const WCHAR *path, reparse_buffer *buffer)
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
    int    result = 0;
    WCHAR *wide   = to_wide (inputPath, &result);

    if (wide == NULL)
        return result;

    result = open_for_query (wide, 0, &file);
    free (wide);

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

int c_full_path (const char *inputPath, char *fullPath, size_t bufsiz)
{
    int    error = 0;
    WCHAR *wide  = decode_path (inputPath, &error);
    WCHAR *full  = NULL;
    DWORD  needed;
    DWORD  length;

    if (wide == NULL)
        return error;

    // The size needed including terminator, or 0 for error
    needed = GetFullPathNameW (wide, 0, NULL, NULL);
    full   = needed == 0 ? NULL : malloc (needed * sizeof (WCHAR));

    if (needed == 0) {
        error = last_error ();
    } else if (full == NULL) {
        error = ERROR_NOT_ENOUGH_MEMORY;
    } else {
        // The length excluding terminator, unless it has grown in the meantime
        length = GetFullPathNameW (wide, needed, full, NULL);

        if (length == 0)
            error = last_error ();
        else if (length >= needed)
            error = ERR_BUFFER_TOO_SMALL;
        else
            error = from_wide (full, length, fullPath, bufsiz);
    }

    free (full);
    free (wide);
    return error;
}

// Whether Windows takes path for a device, which depends on its version. Only
// the start of the result matters, and devices have short names.
static int is_device (const WCHAR *path)
{
    WCHAR full[16];
    DWORD length = GetFullPathNameW (path, 16, full, NULL);

    return length > 0 && length < 16 && wcsncmp (full, L"\\\\.\\", 4) == 0;
}

static int wide_kind (const WCHAR *path)
{
    DWORD attributes = GetFileAttributesW (path);

    if (attributes == INVALID_FILE_ATTRIBUTES) {
        // Files that are always in use, like the pagefile, can't be queried
        if (GetLastError () == ERROR_SHARING_VIOLATION)
            return DEN_FILE;

        return is_device (path) ? DEN_SPECIAL : DEN_NOTHING;
    }

    if (attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
        reparse_buffer buffer;
        int            error = read_reparse_point (path, &buffer);

        if (error == 0) {
            DWORD tag = buffer.data.ReparseTag;

            if (tag == IO_REPARSE_TAG_SYMLINK
                || tag == IO_REPARSE_TAG_MOUNT_POINT)
            {
                return DEN_SOFTLINK;
            }

            // Anything else that stands for another item must not be entered,
            // and these are the Windows counterparts of Unix special files.
            if (IsReparseTagNameSurrogate (tag) || tag == REPARSE_TAG_AF_UNIX
                || tag == REPARSE_TAG_LX_FIFO || tag == REPARSE_TAG_LX_CHR
                || tag == REPARSE_TAG_LX_BLK)
            {
                return DEN_SPECIAL;
            }

            // The remaining tags keep the nature of the item, like the
            // placeholders of OneDrive or deduplicated files.
        } else if (error != ERROR_NOT_A_REPARSE_POINT) {
            return DEN_SPECIAL; // It could be a link, so it must not be entered
        }
    }

    if (attributes & FILE_ATTRIBUTE_DIRECTORY)
        return DEN_DIRECTORY;

    return is_device (path) ? DEN_SPECIAL : DEN_FILE;
}

int c_kind (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);
    int    result;

    if (wide == NULL)
        return DEN_NOTHING;

    result = wide_kind (wide);
    free (wide);
    return result;
}

long long c_file_size (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);
    long long result;
    WIN32_FILE_ATTRIBUTE_DATA data;

    if (wide == NULL)
        return -error;

    if (GetFileAttributesExW (wide, GetFileExInfoStandard, &data))
        result = ((long long) data.nFileSizeHigh << 32) | data.nFileSizeLow;
    else
        result = -last_error ();

    free (wide);
    return result;
}

int c_getcwd (char *buf, size_t bufsiz)
{
    int    result = 0;
    DWORD  needed = GetCurrentDirectoryW (0, NULL); // Including terminator
    WCHAR *path   = needed == 0 ? NULL : malloc (needed * sizeof (WCHAR));

    if (needed == 0) {
        result = last_error ();
    } else if (path == NULL) {
        result = ERROR_NOT_ENOUGH_MEMORY;
    } else {
        DWORD length = GetCurrentDirectoryW (needed, path);

        if (length == 0) {
            result = last_error ();
        } else if (length >= needed) {
            result = ERR_BUFFER_TOO_SMALL; // It has changed in the meantime
        } else {
            size_t       regular_length = length;
            const WCHAR *regular = regular_path (path, &regular_length);

            result = from_wide (regular, regular_length, buf, bufsiz);
        }
    }

    free (path);
    return result;
}

int c_mkdir (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);

    if (wide == NULL)
        return error;

    if (!CreateDirectoryW (wide, NULL))
        error = last_error ();

    free (wide);
    return error;
}

// Calls remove for path, which is either RemoveDirectoryW or DeleteFileW. Unlike
// other systems, Windows refuses to remove read-only items, so that attribute
// has to go first in that case. Returns an error code.
static int remove_item (const WCHAR *path, BOOL (WINAPI *remove) (LPCWSTR))
{
    DWORD attributes;
    int   error;

    if (remove (path))
        return 0;

    error      = last_error ();
    attributes = GetFileAttributesW (path);

    if (error == ERROR_ACCESS_DENIED
        && attributes != INVALID_FILE_ATTRIBUTES
        && (attributes & FILE_ATTRIBUTE_READONLY)
        && SetFileAttributesW (path, attributes & ~FILE_ATTRIBUTE_READONLY))
    {
        return remove (path) ? 0 : last_error ();
    }

    return error;
}

int c_rmdir (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);

    if (wide == NULL)
        return error;

    error = remove_item (wide, RemoveDirectoryW);
    free (wide);
    return error;
}

int c_unlink (const char *path)
{
    int    error = 0;
    WCHAR *wide  = to_wide (path, &error);

    if (wide == NULL)
        return error;

    error = remove_item (wide, DeleteFileW);
    free (wide);
    return error;
}

// The state of a directory listing. The first entry comes along with the search
// handle, which is invalid for an empty search, only possible on an empty drive.
typedef struct {
    HANDLE           find;
    WIN32_FIND_DATAW data;
    int              first;
} dir_listing;

void *c_dir_open (const char *path, int *error)
{
    size_t       length  = strlen (path);
    char        *pattern = malloc (length + 3);
    WCHAR       *wide;
    dir_listing *dir;

    if (pattern == NULL) {
        *error = ERROR_NOT_ENOUGH_MEMORY;
        return NULL;
    }

    // Only a separator is needed, unless there's already one or it's just C:
    strcpy (pattern, path);
    if (length > 0 && path[length - 1] != '\\' && path[length - 1] != '/'
        && !(length == 2 && path[1] == ':'))
    {
        strcat (pattern, "\\");
    }
    strcat (pattern, "*");

    wide = to_wide (pattern, error);
    free (pattern);

    if (wide == NULL)
        return NULL;

    dir = malloc (sizeof (*dir));

    if (dir == NULL) {
        *error = ERROR_NOT_ENOUGH_MEMORY;
    } else {
        dir->first = 1;
        dir->find  = FindFirstFileW (wide, &dir->data);

        if (dir->find == INVALID_HANDLE_VALUE
            && GetLastError () != ERROR_FILE_NOT_FOUND)
        {
            *error = last_error ();
            free (dir);
            dir = NULL;
        }
    }

    free (wide);
    return dir;
}

int c_dir_read (void *handle, char *buf, size_t bufsiz)
{
    dir_listing *dir = handle;

    if (dir->find == INVALID_HANDLE_VALUE) {
        buf[0] = '\0';
        return 0;
    }

    if (dir->first) {
        dir->first = 0;
    } else if (!FindNextFileW (dir->find, &dir->data)) {
        if (GetLastError () != ERROR_NO_MORE_FILES)
            return last_error ();

        buf[0] = '\0';
        return 0;
    }

    return from_wide (dir->data.cFileName, wcslen (dir->data.cFileName),
                      buf, bufsiz);
}

void c_dir_close (void *handle)
{
    dir_listing *dir = handle;

    if (dir->find != INVALID_HANDLE_VALUE)
        FindClose (dir->find);

    free (dir);
}

// A plain copy, as CopyFileW would also bring attributes that were not requested
int c_copy_file (const char *source, const char *target)
{
    int    error       = 0;
    WCHAR *wide_source = to_wide (source, &error);
    WCHAR *wide_target = wide_source == NULL ? NULL : to_wide (target, &error);
    HANDLE from;
    HANDLE to;
    char   buffer[65536];
    DWORD  read;
    DWORD  written;

    if (wide_target == NULL) {
        free (wide_source);
        return error;
    }

    from = CreateFileW (wide_source, GENERIC_READ,
                        FILE_SHARE_READ | FILE_SHARE_WRITE,
                        NULL, OPEN_EXISTING, 0, NULL);

    if (from == INVALID_HANDLE_VALUE) {
        error = last_error ();
    } else {
        to = CreateFileW (wide_target, GENERIC_WRITE, 0,
                          NULL, CREATE_ALWAYS, 0, NULL);

        if (to == INVALID_HANDLE_VALUE) {
            error = last_error ();
        } else {
            while (error == 0) {
                if (!ReadFile (from, buffer, sizeof (buffer), &read, NULL)
                    || (read > 0
                        && !WriteFile (to, buffer, read, &written, NULL)))
                {
                    error = last_error ();
                } else if (read == 0) {
                    break;
                }
            }

            CloseHandle (to);

            if (error != 0) // Don't leave half a file behind
                DeleteFileW (wide_target);
        }

        CloseHandle (from);
    }

    free (wide_source);
    free (wide_target);
    return error;
}

// Copies the last access and last write times from one open item to another
static int copy_times (const WCHAR *source, const WCHAR *target)
{
    FILETIME accessed;
    FILETIME modified;
    HANDLE   from;
    HANDLE   to;
    int      error = open_for_query (source, 0, &from);

    if (error != 0)
        return error;

    if (!GetFileTime (from, NULL, &accessed, &modified)) {
        error = last_error ();
    } else {
        // Backup semantics are needed to open directories
        to = CreateFileW (target, FILE_WRITE_ATTRIBUTES,
                          FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                          NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

        if (to == INVALID_HANDLE_VALUE) {
            error = last_error ();
        } else {
            if (!SetFileTime (to, NULL, &accessed, &modified))
                error = last_error ();

            CloseHandle (to);
        }
    }

    CloseHandle (from);
    return error;
}

int c_copy_attributes (const char *source, const char *target,
                       int permissions, int timestamps)
{
    int    error       = 0;
    WCHAR *wide_source = to_wide (source, &error);
    WCHAR *wide_target = wide_source == NULL ? NULL : to_wide (target, &error);

    if (wide_target != NULL) {
        if (timestamps)
            error = copy_times (wide_source, wide_target);

        if (error == 0 && permissions) {
            DWORD attributes = GetFileAttributesW (wide_source);

            if (attributes == INVALID_FILE_ATTRIBUTES
                || !SetFileAttributesW (wide_target, attributes))
            {
                error = last_error ();
            }
        }
    }

    free (wide_source);
    free (wide_target);
    return error;
}

int c_link_target (const char *path, char *buf, size_t bufsiz)
{
    reparse_buffer buffer;
    const WCHAR   *target;
    size_t         length;
    int            error = 0;
    WCHAR         *wide  = to_wide (path, &error);

    if (wide == NULL)
        return error;

    error = read_reparse_point (wide, &buffer);
    free (wide);

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
    } else if (wide_kind (wide) != DEN_SOFTLINK) {
        error = ERROR_NOT_A_REPARSE_POINT;
    } else {
        // Directory links are directories
        error = remove_item (wide, (attributes & FILE_ATTRIBUTE_DIRECTORY)
                                   ? RemoveDirectoryW : DeleteFileW);
    }

    free (wide);
    return error;
}

// Allows unprivileged users to create links in Developer Mode, but systems that
// predate this flag reject it. Returns an error code.
static int create_symlink (const WCHAR *name, const WCHAR *target, int is_dir)
{
    DWORD flags = is_dir ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0;

    if (CreateSymbolicLinkW
            (name, target, flags | SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE))
        return 0;

    if (GetLastError () == ERROR_INVALID_PARAMETER
        && CreateSymbolicLinkW (name, target, flags))
        return 0;

    return last_error ();
}

// A junction is a directory that holds this data, which takes no privileges,
// and whose target is always absolute. Returns an error code.
static int create_junction (const WCHAR *name, reparse_buffer *buffer)
{
    const DWORD header = 8; // The fields before the data that the length counts
    DWORD  returned;
    HANDLE file;
    int    error = 0;

    if (!CreateDirectoryW (name, NULL))
        return last_error ();

    file = CreateFileW
        (name, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
         FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (file == INVALID_HANDLE_VALUE) {
        error = last_error ();
    } else {
        if (!DeviceIoControl (file, FSCTL_SET_REPARSE_POINT, buffer,
                              header + buffer->data.ReparseDataLength,
                              NULL, 0, &returned, NULL))
            error = last_error ();

        CloseHandle (file);
    }

    if (error != 0)
        RemoveDirectoryW (name); // Don't leave a plain directory in its place

    return error;
}

int c_copy_link (const char *source, const char *name)
{
    int            error       = 0;
    WCHAR         *wide_source = to_wide (source, &error);
    WCHAR         *wide_name   = wide_source == NULL ? NULL
                                                     : to_wide (name, &error);
    reparse_buffer buffer;

    if (wide_name != NULL)
        error = read_reparse_point (wide_source, &buffer);

    if (wide_name != NULL && error == 0) {
        if (buffer.data.ReparseTag == IO_REPARSE_TAG_MOUNT_POINT) {
            error = create_junction (wide_name, &buffer);
        } else {
            const WCHAR *target;
            size_t       length;

            error = stored_target (&buffer, &target, &length);

            if (error == 0) {
                // Stored targets are not terminated
                WCHAR *terminated = malloc ((length + 1) * sizeof (WCHAR));

                if (terminated == NULL) {
                    error = ERROR_NOT_ENOUGH_MEMORY;
                } else {
                    wcsncpy (terminated, target, length);
                    terminated[length] = 0;

                    // The kind of link is that of the original, even if broken
                    error = create_symlink
                        (wide_name, terminated,
                         GetFileAttributesW (wide_source)
                         & FILE_ATTRIBUTE_DIRECTORY);

                    free (terminated);
                }
            }
        }
    }

    free (wide_source);
    free (wide_name);
    return error;
}

int c_create_link (const char *target, const char *name, int is_dir)
{
    int    error       = 0;
    WCHAR *wide_target = decode_path (target, &error); // Stored as given
    WCHAR *wide_name   = wide_target == NULL ? NULL : to_wide (name, &error);

    if (wide_name != NULL) {
        // Targets are stored as given, and the OS does not take a stored '/'
        // as a separator, so such a link would be broken
        for (WCHAR *c = wide_target; *c != 0; c++) {
            if (*c == L'/')
                *c = L'\\';
        }

        error = create_symlink (wide_name, wide_target, is_dir);
    }

    free (wide_target);
    free (wide_name);
    return error;
}

#else

// ISO C forbids an empty translation unit
typedef int denc_windows_not_applicable;

#endif
