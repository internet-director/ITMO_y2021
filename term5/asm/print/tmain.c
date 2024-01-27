#include <stdio.h>
#include <stdlib.h>

void print(char *out_buf, const char *format, const char *hex_number);

char *tests[][3] =
        {
                {"", "0", "0"},
                {"4", "10", "  16"},
                {"-4", "-10", "-16 "},
                {"+", "10", "+16"},
                {" ", "10", " 16"},
                {"+-3", "8Ee4fACF834B20a0b0DE134F630E7342", "-150343060792470555638386732992063179966"},
        };

int main(void)
{
    int passed = 0, all = sizeof(tests)/sizeof(*tests);
    for (int t = 0; t < all; t++)
    {
        printf("format '%s', number '%s', expected '%s'", tests[t][0], tests[t][1], tests[t][2]);
        char buf[50];
        print(buf, tests[t][0], tests[t][1]);
        int ok = !strcmp(buf, tests[t][2]);
        printf(", got '%s', result: %s\n", buf, ok ? "OK" : "FAIL");
        passed += ok;
    }
    return all - passed;
}