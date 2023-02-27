#include "kernel/types.h"
#include "kernel/syscall.h"
#include "user/user.h"

#ifdef SYS_dump
void test1();
void test2();
void test3();
void test4();
#endif

int main(void) {
  printf("dump tests started\n");
#ifndef SYS_dump
  printf("no dump syscall found. Stop testing\n");
  goto no_dump;
#endif
#ifdef SYS_dump
  printf("dump syscall found. Start testing\n");
  test1();
  test2();
  test3();
  test4();
  printf("4 tests were ran\n");
#endif
#ifndef SYS_dump
no_dump:
#endif
  exit(0);
}

#ifdef SYS_dump

void test1() {
  printf("#####################\n");
  printf("#                   #\n");
  printf("#   initial state   #\n");
  printf("#                   #\n");
  printf("#####################\n");
  dump();
}

int dump_test2_asm();

void test2() {
  printf("#####################\n");
  printf("#                   #\n");
  printf("#       test 1      #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("#                   #\n");
  printf("#  expected values  #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("# s2  = 2           #\n");
  printf("# s3  = 3           #\n");
  printf("# s4  = 4           #\n");
  printf("# s5  = 5           #\n");
  printf("# s6  = 6           #\n");
  printf("# s7  = 7           #\n");
  printf("# s8  = 8           #\n");
  printf("# s9  = 9           #\n");
  printf("# s10 = 10          #\n");
  printf("# s11 = 11          #\n");
  printf("#####################\n");
  dump_test2_asm();
}

int dump_test3_asm();

void test3() {
  printf("#####################\n");
  printf("#                   #\n");
  printf("#      test 2       #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("#                   #\n");
  printf("#  expected values  #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("# s2 = 1            #\n");
  printf("# s3 = -12          #\n");
  printf("# s4 = 123          #\n");
  printf("# s5 = -1234        #\n");
  printf("# s6 = 12345        #\n");
  printf("# s7 = -123456      #\n");
  printf("# s8 = 1234567      #\n");
  printf("# s9 = -12345678    #\n");
  printf("# s10 = 123456789   #\n");
  printf("# s11 = -1234567890 #\n");
  printf("#####################\n");
  dump_test3_asm();
}

int dump_test4_asm();

void test4() {
  printf("#####################\n");
  printf("#                   #\n");
  printf("#      test 3       #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("#                   #\n");
  printf("#  expected values  #\n");
  printf("#                   #\n");
  printf("#####################\n");
  printf("# s2 = 2147483647   #\n");
  printf("# s3 = -2147483648  #\n");
  printf("# s4 = 1337         #\n");
  printf("# s5 = 2020         #\n");
  printf("# s6 = 3234         #\n");
  printf("# s7 = 3235         #\n");
  printf("# s8 = 3236         #\n");
  printf("# s9 = 3237         #\n");
  printf("# s10 = 3238        #\n");
  printf("# s11 = 3239        #\n");
  printf("#####################\n");
  dump_test4_asm();
}
#endif
