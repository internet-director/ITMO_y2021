#include "kernel/syscall.h"
.globl dump_test2_asm
dump_test2_asm:
  li s2, 2
  li s3, 3
  li s4, 4
  li s5, 5
  li s6, 6
  li s7, 7
  li s8, 8
  li s9, 9
  li s10, 10
  li s11, 11
#ifdef SYS_dump
  li a7, SYS_dump
  ecall
#endif
  ret
.globl dump_test3_asm
dump_test3_asm:
  li s2, 1
  li s3, -12
  li s4, 123
  li s5, -1234
  li s6, 12345
  li s7, -123456
  li s8, 1234567
  li s9, -12345678
  li s10, 123456789
  li s11, -1234567890
#ifdef SYS_dump
  li a7, SYS_dump
  ecall
#endif
  ret
.globl dump_test4_asm
dump_test4_asm:
  li s2, 2147483647
  li s3, -2147483648
  li s4, 1337
  li s5, 2020
  li s6, 3234
  li s7, 3235
  li s8, 3236
  li s9, 3237
  li s10, 3238
  li s11, 3239
#ifdef SYS_dump
  li a7, SYS_dump
  ecall
#endif
  ret
