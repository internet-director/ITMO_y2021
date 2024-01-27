#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <intrin.h>

#define TST "../"

#define LZW_MAXBITS                 12
#define LZW_SIZTABLE                (1<<LZW_MAXBITS)

struct LZWState {
    int bbits;
    unsigned int bbuf;

    int cursize;
    int top_slot;
    int slot;
    int fc, oc;
    uint8_t* sp;
    uint8_t stack[LZW_SIZTABLE];
    uint8_t suffix[LZW_SIZTABLE];
    uint16_t prefix[LZW_SIZTABLE];
};

size_t lzw_decode2(const uint8_t* in, size_t in_size, uint8_t* out, size_t out_size) {
    if (in == NULL || out == NULL) {
        return -1;
    }

    struct LZWState p;
    p.bbuf = 0;
    p.bbits = 0;

    p.cursize = 9;
    p.top_slot = 512;
    p.slot = 258;
    p.oc = p.fc = -1;
    p.sp = p.stack;

    int l, c, code;
    l = out_size;

    uint32_t bbuf_high = 0, bbuf_low = 0;
    int k = 0;

    uint8_t* ptr = out;

    for (;;) {
        {
            if (in_size > 0 && p.bbits < p.cursize) {
                if (in_size >= 4) {
                    p.bbits += 32;
                    in_size -= 4;

                    bbuf_high = bbuf_low;
                    
                    bbuf_low = _byteswap_ulong(*((uint32_t*)in));

                    in += 4;
                }
                else {
                    in_size *= 8;
                    p.bbits += in_size;
                    bbuf_high = bbuf_low >> (32 - in_size);
                    bbuf_low <<=  in_size;

                    for (int i = in_size - 8; i >= 0; i-=8) {
                        bbuf_low |= (*in++ << i);
                    }

                    in_size = 0;
                }

            }

            p.bbits -= p.cursize;

            c = (bbuf_low >> p.bbits);
            if (p.bbits != 0) {
                c |= (bbuf_high << (32 - p.bbits));
            }
            c &= (p.top_slot - 1);
        }

        if (c == 257) {
            break;
        }
        else if (c == 256) {
            p.cursize = 9;
            p.slot = 258;
            p.top_slot = 512;
            p.fc = p.oc = -1;
        }
        else {
            code = c;
            if (code == p.slot && p.fc >= 0) {
                *p.sp++ = p.fc;
                code = p.oc;
            }
            else if (code >= p.slot)
                break;
            while (code >= 258) {
                *p.sp++ = p.suffix[code];
                code = p.prefix[code];
            }
            *p.sp++ = code;

            p.fc = code;

            while (p.sp > p.stack) {
                *out++ = *(--p.sp);
                if ((--l) == 0)
                    return out_size;
            }

            if (p.slot < p.top_slot && p.oc >= 0) {
                p.suffix[p.slot] = code;
                p.prefix[p.slot++] = p.oc;
            }
            p.oc = c;
            if (p.slot + 1 >= p.top_slot) {
                if (p.cursize < LZW_MAXBITS) {
                    p.cursize++;
                    p.top_slot <<= 1;
                }
            }
        }
    }
    return out_size - l;
}

extern size_t lzw_decodes(const uint8_t* in, size_t in_size, uint8_t* out, size_t out_size);

int get_file(const char* fname, uint8_t** buffer, size_t* buffer_size)
{
    int exit_code = 0;
    FILE* fd = fopen(fname, "rb");
    if (!fd)
    {
        exit_code = -2; goto clean;
    }
    fseek(fd, 0, SEEK_END);
    *buffer_size = ftell(fd);
    fseek(fd, 0, SEEK_SET);
    *buffer = malloc(sizeof(uint8_t) * *buffer_size);
    if (!*buffer)
    {
        exit_code = -3; goto clean;
    }
    size_t read_size = fread(*buffer, sizeof(uint8_t), *buffer_size, fd);
    if (read_size != *buffer_size)
    {
        fprintf(stderr, "read_size != *buffer_size -- %zd != %zd", read_size, *buffer_size);
        exit_code = -4; goto clean;
    }
clean:
    if (fd)
        fclose(fd);
    return exit_code;
}

int test(const char* fin, const char* fref)
{
    int exit_code = 0;
    uint8_t* in = NULL, * out = NULL, * ref = NULL;
    size_t in_size = 0, ref_size = 0;

    if ((exit_code = get_file(fin, &in, &in_size)))
    {
        if (exit_code == -2)
            fprintf(stderr, "[FATAL ERROR] '%s' cannot opened!", fin);
        else if (exit_code == -3)
            fprintf(stderr, "[FATAL ERROR] not enought memory");
        else
            fprintf(stderr, "[FATAL ERROR] error during reading '%s'", fin);
        goto clean_all;
    }
    if ((exit_code = get_file(fref, &ref, &ref_size)))
    {
        if (exit_code == -2)
            fprintf(stderr, "[FATAL ERROR] '%s' cannot opened!", fref);
        else if (exit_code == -3)
            fprintf(stderr, "[FATAL ERROR] not enought memory");
        else
            fprintf(stderr, "[FATAL ERROR] error during reading '%s'", fref);
        goto clean_all;
    }
    out = malloc(sizeof(uint8_t) * ref_size);
    if (!out)
    {
        exit_code = -3; goto clean_all;
    }

    //memset(out, -1, ref_size);

    size_t retcode = lzw_decodes(in, in_size, out, ref_size);

    if (retcode == -1)
    {
        fprintf(stderr, "[ERROR] retcode lzw_decode() == -1");
        exit_code = -1;	goto clean_all;
    }
    if (retcode != ref_size)
    {
        fprintf(stderr, "[ERROR] retcode lzw_decode() != expected count bytes - %d != %d", retcode, ref_size);
        exit_code = -1;	goto clean_all;
    }

    exit_code = memcmp(ref, out, ref_size);

    //printf("%s\n", out);
    //printf(ref);
    if (exit_code)
    {
        fprintf(stderr, "[ERROR] out != expected");
        exit_code = -1;	goto clean_all;
    }

    unsigned int dummy;
    volatile uint64_t tr = __rdtscp(&dummy);
    for (unsigned int i = 0; i < 10000; i++)
        lzw_decodes(in, in_size, out, ref_size);
    tr = __rdtscp(&dummy) - tr;
    printf("Time '%s': %llu\n", fin, tr / 1000000);

clean_all:
    if (in)
        free(in);
    if (out)
        free(out);
    if (ref)
        free(ref);
    return exit_code;
}

int main(void)
{
    unsigned int x = 6;
    unsigned int y = 32 - x;
     
    unsigned int z = x + 0xffffffff;
    

    int exit_code;
    exit_code = test(TST "test_data/in1", TST "test_data/ref1");
    if (!exit_code)
        exit_code = test(TST "test_data/in2", TST "test_data/ref2");
    return exit_code;
}