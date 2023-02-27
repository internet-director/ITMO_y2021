#include "kernel/types.h"
#include "kernel/stat.h"
#include "user/user.h"

int main(int argc, char *argv[]) {
  int p[2];
  pipe(p);
  int pid = fork(), code = -1;
  close(0);

  if (pid > 0) {
    if (write(p[1], "ping", 4) != 4) {
      printf("Cant write data in pipe!\n");
      goto end;
    }

    char c[513];
    int readed = 0;
    while (readed < 4) {
      int tmp = read(p[0], c + readed, 512 - readed);
      if (tmp < 0) {
        printf("Cant read data from pipe!\n");
        goto end;
      }
      readed += tmp;
    }
    if (readed > 4) {
      c[readed] = 0;
      printf("Invalid readed size!\nData: %s\n", c);
      goto end;
    }
    c[4] = 0;

    printf("%d: got %s\n", getpid(), c);
    code = 0;
    wait(0);
  } else if (pid == 0) {
    char c[513];
    int readed = 0;
    while (readed < 4) {
      int tmp = read(p[0], c + readed, 512 - readed);
      if (tmp < 0) {
        printf("Cant read data from pipe!\n");
        goto fork_end;
      }
      readed += tmp;
    }
    if (readed > 4) {
      c[readed] = 0;
      printf("Invalid readed size!\nData: %s\n", c);
      goto fork_end;
    }
    c[4] = 0;

    printf("%d: got %s\n", getpid(), c);
    if (write(p[1], "pong", 4) != 4) {
      printf("Cant write data in pipe!\n");
      goto fork_end;
    }
    code = 0;
  fork_end:;
    exit(code);
  } else {
    printf("Fork error\n");
  }

end:;
  close(p[0]);
  close(p[1]);
  exit(code);
}
