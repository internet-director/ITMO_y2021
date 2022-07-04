#include "utils.h"

namespace mem {
    void memcpy(void* dst, const void* src, size_t size)
    {
        byte* from = (byte*)src;
        if (dst && src && size) {
            for (size_t i = 0; i < size; i++) ((byte*)dst)[i] = from[i];
        }
    }

    void memset(void* dst, const byte value, size_t size) {
        if (dst && size) {
            for (size_t i = 0; i < size; i++) ((byte*)dst)[i] = value;
        }
    }

    int memcmp(const void* a, const void* b, size_t sz)
    {
        for (size_t i = 0; i < sz; i++) {
            if (((byte*)a)[i] != ((byte*)b)[i]) {
                return (((byte*)a)[i] > ((byte*)b)[i]) ? 1 : -1;
            }
        }
        return 0;
    }
}

size_t str_length(const char* data)
{
    size_t size = -1;

    while (data[++size]);
    return size;
}