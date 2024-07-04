#include <filesystem>
#include <cstdio>
#include <windows.h>
#include <iostream>
#include <vector>

namespace fs = std::filesystem;

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
}

// Returns 0 on success, or a negative Windows error code on failure. Fills buf with
// the target of the softlink given in path.
extern "C" int c_link_target(const char *path, char *buf, size_t bufsiz) {
}
