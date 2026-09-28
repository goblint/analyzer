#include <stdio.h>
#include <pthread.h>

// A stream that may be stdout or a stream from fopen is locked by the library
// whichever it is, and a library call writes only the bytes of the buffer, not
// what the buffer's contents point to.
static char buf[BUFSIZ];
int shared;
static struct { int *p; char rest[BUFSIZ]; } err;
FILE *out;

void *t_fun(void *arg) {
  fputs("t", out); // NORACE
  fputs("t", stderr); // NORACE
  return 0;
}

int main(int argc, char **argv) {
  setvbuf(stdout, buf, _IOFBF, sizeof buf);
  err.p = &shared;
  setvbuf(stderr, (char *)&err, _IOFBF, sizeof err);
  out = argc > 1 ? fopen("/dev/null", "w") : stdout;
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  printf("m"); // NORACE
  fputs("m", stderr); // NORACE
  shared = 1; // NORACE
  pthread_join(id, 0);
  return 0;
}
