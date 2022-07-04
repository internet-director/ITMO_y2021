#pragma once
#include <iostream>
typedef unsigned char byte;

namespace mem {
    void memcpy(void* dst, const void* src, size_t size);
    void memset(void* dst, const byte value, size_t size);
    int memcmp(const void* a, const void* b, size_t sz);
}

size_t str_length(const char* data);