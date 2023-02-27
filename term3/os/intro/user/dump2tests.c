#include "kernel/types.h"
#include "kernel/syscall.h"
#include "user/user.h"

#ifdef SYS_dump2
int success;

void test1();
void test2();
void test3();
void test4();
#endif

int main(void) {
  printf("dump2 tests started\n");
#ifndef SYS_dump2
  printf("no dump2 syscall found. Stop testing\n");
  goto no_dump2;
#endif
#ifdef SYS_dump2
  printf("dump2 syscall found. Start testing\n");
  success = 0;
  test1();
  test2();
  test3();
  test4();
  printf("4 tests were run. %d tests passed\n", success);
#endif
#ifndef SYS_dump2
no_dump2:
#endif
  exit(0);
}

#ifdef SYS_dump2
int dump2_test1_asm(int pipefd, char *str, int len);

void test1() {
  printf("test 1 started\n");
  int pipefd[2];
  pipe(pipefd);
  int child_proc = fork();
  if (child_proc == 0) {
    uint64 a = 34381;
    dump2_test1_asm(pipefd[1], (char *)(&a), 8);
  } else {
    uint64 a;
    read(pipefd[0], &a, 8);
    for (int i = 2; i < 12; i++) {
      uint64 value;
      int error = dump2(child_proc, i, &value);
      if (error != 0) {
        printf("[ERROR] dump2 returned unexpected error %d\n", error);
        goto failed;
      }
      if (value != i) {
        printf("[ERROR] expected: %d, found: %d\n", i, value);
        goto failed;
      }
    }
  }
  printf("[SUCCESS] test 1 passed\n");
  success++;
failed:
  if (child_proc > 0) kill(child_proc);
  printf("test 1 finished\n");
}

int dump2_test2_asm(int pipefd, char *str, int len);

void test2() {
  printf("test 2 started\n");
  int pipefd[2];
  pipe(pipefd);
  int child_proc = fork();
  if (child_proc == 0) {
    uint64 a = 34381;
    dump2_test2_asm(pipefd[1], (char *)(&a), 8);
  } else {
    uint64 a;
    read(pipefd[0], &a, 8);
    for (int i = 2; i < 12; i++) {
      uint64 value;
      int error = dump2(child_proc, i, &value);
      if (error != 0) {
        printf("[ERROR] dump2 returned unexpected error %d\n", error);
        goto failed;
      }
      if (value != i * i) {
        printf("[ERROR] expected: %d, found %d\n", i * i, value);
        goto failed;
      }
    }
  }
  printf("[SUCCESS] test 2 passed\n");
  success++;
failed:
  if (child_proc > 0) kill(child_proc);
  printf("test 2 finished\n");
}

int dump2_test3_asm(int pid, uint64 *ptr);

void test3() {
  printf("test 3 started\n");
  uint64 value;
  int result = dump2_test3_asm(getpid(), &value);
  if (result != 0) {
    printf("[ERROR] dump2 returned unexpected error %d\n", result);
    goto failed;
  }
  if (value != 1337) {
    printf("[ERROR] expected: 1337, found: %d", value);
    goto failed;
  }
  printf("[SUCCESS] test 3 passed\n");
  success++;
failed:
  printf("test 3 finished\n");
}

void test4() {
  printf("test 4 started\n");
  uint64 a;
  printf("[INFO] testing nonexisting proccess\n");
  int error = dump2(2147483647, 10, &a);
  if (error != -2) {
    printf("[ERROR] dump2 returned unexpected value %d, expected -2\n", error);
    goto failed;
  }
  printf("[OK] nonexisting proccess\n");
  printf("[INFO] testing illegal access to registers\n");
  int pipefd[2];
  pipe(pipefd);
  int parent_pid = getpid();
  int child_proc = fork();
  if (child_proc == 0) {
    error = dump2(parent_pid, 10, &a);
    write(pipefd[1], &error, 4);
    exit(0);
  } else {
    read(pipefd[0], &error, 4);
    if (error != -1) {
      printf("[ERROR] dump2 returned unexpected value %d, expected -1\n",
             error);
      goto failed;
    }
  }
  printf("[OK] illegal access to registers\n");
  printf("[INFO] testing incorrect number of register\n");
  error = dump2(parent_pid, 1337, &a);
  if (error != -3) {
    printf("[ERROR] dump2 returned unexpected value %d, expected -3\n", error);
    goto failed;
  }
  printf("[OK] incorrect number of register\n");
  printf("[INFO] testing invalid memory address\n");
  error = dump2(parent_pid, 10, &a + 123456789);
  if (error != -4) {
    printf("[ERROR] dump2 returned unexpected value %d, expected -4\n", error);
    goto failed;
  }
  printf("[OK] invalid memory address\n");
  printf("[SUCCESS] test 4 passed\n");
  success++;
failed:
  printf("test 4 finished\n");
}
#endif
