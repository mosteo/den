#define UNICODE
#define _UNICODE
#include <windows.h>
#include <winioctl.h>

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

int is_softlink_c(const char *path) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;
    WCHAR wpath[MAX_PATH];

    if (MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH) == 0) {
        return -GetLastError();
    }

    hFile = CreateFileW(wpath, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                        NULL, OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);

    if (hFile == INVALID_HANDLE_VALUE) {
        DWORD error = GetLastError();
        // If the file doesn't exist or we don't have access, it's not a symlink
        if (error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND || error == ERROR_ACCESS_DENIED) {
            return 0;
        }
        return -error;
    }

    if (!DeviceIoControl(hFile, FSCTL_GET_REPARSE_POINT, NULL, 0, reparseBuffer,
                         MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &dwRetLen, NULL)) {
        DWORD error = GetLastError();
        CloseHandle(hFile);
        // If it's not a reparse point, it's not a symlink
        if (error == ERROR_NOT_A_REPARSE_POINT) {
            return 0;
        }
        return -error;
    }

    CloseHandle(hFile);

    // Check if it's specifically a symlink
    return (reparseData->ReparseTag == IO_REPARSE_TAG_SYMLINK) ? 1 : 0;
}

// Returns the length of the information stored in a softlink, or a negative Windows error code on failure
LONG link_len(const char *path) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;
    WCHAR wpath[MAX_PATH];

    if (MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH) == 0) {
        return -GetLastError();
    }

    hFile = CreateFileW(wpath, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
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

// Returns 0 on success, or a negative Windows error code on failure. Fills buf with
// the target of the softlink given in path.
LONG link_target(const char *path, char *buf, DWORD bufsiz) {
    HANDLE hFile;
    DWORD dwRetLen;
    BYTE reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
    PREPARSE_DATA_BUFFER reparseData = (PREPARSE_DATA_BUFFER)reparseBuffer;
    WCHAR wpath[MAX_PATH];

    if (MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH) == 0) {
        return -GetLastError();
    }

    hFile = CreateFileW(wpath, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
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
