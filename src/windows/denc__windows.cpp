#include <filesystem>
#include <cstdio>
#include <windows.h>
#include <iostream>
#include <vector>

#include "denc.h"

namespace fs = std::filesystem;

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

//  Resolve a link or ""
std::string resolve_link(const char * path, size_t bufsiz) {
    // Using the Windows API, get the softlink target

    // initialize a std::string of bufsiz length
    std::string buffer(bufsiz, '\0');
    DWORD dwRes = GetFinalPathNameByHandle(
        CreateFileA(
            path,
            GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL),
        buffer.data(), MAX_PATH, VOLUME_NAME_DOS);
    if (dwRes == 0) {
        std::cerr << "Error resolving link: " << GetLastError()
        << " for path: " << path << std::endl;
        return "";
    } else {
        // Remove any stupid \\?\ prefix
        if (buffer.size() >= 4 && buffer.substr(0, 4) == "\\\\?\\") {
            buffer = buffer.substr(4);
        }

        std::cout << "Resolved link: " << buffer << std::endl;
        return buffer;
    }
}

//  Returns 0 for success, -1 for not enough buffer, >=1 for non-recoverable error
extern "C" int c_canonical(const char* inputPath, char* fullPath, size_t bufsiz) {
    try {
        // If the path is a non-broken soft-link, fully resolve it and return
        // the target canonical path.
        if (c_is_softlink(inputPath)) {
            std::string resolved = resolve_link(inputPath, bufsiz);
            if (resolved != "") {
                if (resolved.size() >= bufsiz) {
                    return -1;
                }
                std::copy(resolved.begin(), resolved.end(), fullPath);
                fullPath[resolved.size()] = '\0'; // Fscking low-level garbage
                return 0;
            }
        }

        //  From here on, we are dealing with a broken link or regular path

        std::string path = fs::weakly_canonical(inputPath).string();
        if (path.size() >= bufsiz - 1) {
            return -1;
        }
        // copy from path to fullPath with null terminator
        std::copy(path.begin(), path.end(), fullPath);
        fullPath[path.size()] = '\0'; // Fscking low-level garbage
        // print debug obtained string
        std::cout << "Canonical path: " << fullPath << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Error getting canonical path: " << e.what() << std::endl;
        return 1;
    }
}

extern "C" int c_is_softlink(const char *path) {
    // fs::is_symlink, which would be ideal, is lying for some reason

    DWORD attributes = GetFileAttributesW(char_to_wstring(path).c_str());
    if (attributes == INVALID_FILE_ATTRIBUTES) {
        std::cerr << "Error getting file attributes: " << GetLastError() << std::endl;
        return false;
    }
    return (attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;

    return fs::is_symlink(path);
}


// Returns the length of the information stored in a softlink, or a negative Windows error code on failure
extern "C" int c_link_len(const char *path) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;

    hFile = CreateFileW(char_to_wstring(path).c_str(), 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
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

    hFile = CreateFileW(char_to_wstring(path).c_str(), 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
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
