#include <cstdio>
#include <stdexcept>
#include <string>
#include <vector>
#include <windows.h>

#include "denc.h"

typedef struct _REPARSE_DATA_BUFFER {
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
    } DUMMYUNIONNAME;
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

std::wstring char_to_wstring(const char* cstr) {
    if (cstr == nullptr) {
        throw std::invalid_argument("Null pointer provided");
    }

    std::mbstate_t state = std::mbstate_t();
    size_t length = std::mbsrtowcs(nullptr, &cstr, 0, &state);
    if (length == static_cast<size_t>(-1)) {
        throw std::runtime_error("Error converting to wide string");
    }

    std::vector<wchar_t> buffer(length + 1); // +1 for the null terminator
    std::mbsrtowcs(buffer.data(), &cstr, buffer.size(), &state);
    return std::wstring(buffer.data());
}

//  Returns 0 for success, -1 for not enough buffer, >=1 for non-recoverable error
extern "C" int c_canonical(const char* inputPath, char* fullPath, size_t bufsiz) {
    try {
        // Obtain the file handle
        HANDLE hFile = CreateFileA(
                inputPath,
                GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

        if (hFile == INVALID_HANDLE_VALUE) {
            return 1;
        }

        // initialize a std::string of bufsiz length
        std::string buffer(bufsiz, '\0');

        // Returns buffer used or needed, or 0 for error
        DWORD dwRes = GetFinalPathNameByHandle(
            hFile,
            const_cast<LPSTR>(buffer.c_str()), bufsiz, VOLUME_NAME_DOS);
            // NOTE: recursive links resolve to themselves in Windows!!

        // Close the file handle, no longer needed
        CloseHandle(hFile);

        if (dwRes == 0) {
            // Links is broken
            return 1;
        } else if (dwRes >= bufsiz) {
            // Not enough buffer
            return -1;
        } else {
            // Remove any stupid \\?\ prefix
            if (buffer.size() >= 4 && buffer.substr(0, 4) == "\\\\?\\") {
                buffer = buffer.substr(4);
            }
            // Copy from buffer to fullPath
            strcpy(fullPath, buffer.c_str());
            return 0;
        }
    } catch (const std::exception& e) {
        // print to stderr
        fprintf(stderr, "Error getting canonical path: %s\n", e.what());
        return 1;
    }
}

extern "C" int c_is_softlink(const char *path) {
    // fs::is_symlink, which would be ideal, is lying for some reason

    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;

    hFile = CreateFileA(path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        NULL, OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (hFile == INVALID_HANDLE_VALUE) {
        return 0;
    }

    if (!DeviceIoControl(hFile, FSCTL_GET_REPARSE_POINT, NULL, 0, reparseBuffer,
                         MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &dwRetLen, NULL)) {
        CloseHandle(hFile);
        return 0;
    }

    CloseHandle(hFile);

    return reparseData->ReparseTag == IO_REPARSE_TAG_SYMLINK ? 1 : 0;
}


// Returns the length of the information stored in a softlink, or a negative Windows error code on failure
extern "C" int c_link_len(const char *path) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;

    hFile = CreateFileA(path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        NULL, OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (hFile == INVALID_HANDLE_VALUE) {
        return -GetLastError();
    }

    if (!DeviceIoControl(hFile, FSCTL_GET_REPARSE_POINT, NULL, 0, reparseBuffer,
                         MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &dwRetLen, NULL)) {
        DWORD error = GetLastError();
        CloseHandle(hFile);
        return -error;
    }

    CloseHandle(hFile);

    if (reparseData->ReparseTag != IO_REPARSE_TAG_SYMLINK) {
        return -ERROR_NOT_A_REPARSE_POINT;
    }

    return reparseData->SymbolicLinkReparseBuffer.PrintNameLength / sizeof(WCHAR);
}

// Returns 0 on success, -1 if not enought buffer, or a positive error code on failure. Fills buf with
// the target of the softlink given in path.
extern "C" int c_link_target(const char *path, char *buf, size_t bufsiz) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;

    hFile = CreateFileA(path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        NULL, OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (hFile == INVALID_HANDLE_VALUE) {
        return -GetLastError();
    }

    if (!DeviceIoControl(hFile, FSCTL_GET_REPARSE_POINT, NULL, 0, reparseBuffer,
                         MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &dwRetLen, NULL)) {
        DWORD error = GetLastError();
        CloseHandle(hFile);
        return -error;
    }

    CloseHandle(hFile);

    if (reparseData->ReparseTag != IO_REPARSE_TAG_SYMLINK) {
        return -ERROR_NOT_A_REPARSE_POINT;
    }

    PWCHAR targetPath = reparseData->SymbolicLinkReparseBuffer.PathBuffer +
                        (reparseData->SymbolicLinkReparseBuffer.PrintNameOffset / sizeof(WCHAR));
    DWORD targetLength = reparseData->SymbolicLinkReparseBuffer.PrintNameLength / sizeof(WCHAR);

    if (WideCharToMultiByte(CP_UTF8, 0, targetPath, targetLength, buf, bufsiz - 1, NULL, NULL) == 0) {
        return -GetLastError();
    }

    buf[targetLength] = '\0';  // Null-terminate the string
    return 0;  // Success
}

extern "C" int
c_copy_link (const char *target, const char *name)
{
    DWORD flags = 0;

    // We must explicitly mark as directory if source link is one
    if (GetFileAttributesA(target) & FILE_ATTRIBUTE_DIRECTORY)
    {
        flags |= SYMBOLIC_LINK_FLAG_DIRECTORY;
    }

    // Use the newer version that allows unprivileged users to create symlinks
    flags |= SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;

    BOOLEAN result = CreateSymbolicLinkA(name, target, flags);

    // Return 0 on success, -1 on failure to match Unix symlink() behavior
    return (result == TRUE) ? 0 : -1;
}

extern "C" int
c_delete_link (const char *path)
{
    // Use RemoveDirectoryA for directory symlinks and DeleteFileA for file
    // symlinks

    DWORD attrs = GetFileAttributesA(path);
    if (attrs == INVALID_FILE_ATTRIBUTES)
    {
        return -1; // Error: file/directory not found
    }

    // Check if it's a reparse point (which includes symlinks)
    if (!(attrs & FILE_ATTRIBUTE_REPARSE_POINT))
    {
        return -1; // Error: not a symlink
    }

    BOOL result;
    if (attrs & FILE_ATTRIBUTE_DIRECTORY)
    {
        result = RemoveDirectoryA(path);
    }
    else
    {
        result = DeleteFileA(path);
    }

    return (result != 0) ? 0 : -1;
}
