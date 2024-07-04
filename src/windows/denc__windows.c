#define UNICODE
#define _UNICODE
#include <windows.h>

// Returns the length of the information stored in a softlink, or a negative Windows error code on failure
LONG link_len(const char *path) {
    HANDLE hFile;
    DWORD dwRetLen;
    char reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
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
    char reparseBuffer[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
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