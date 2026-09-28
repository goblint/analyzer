#include <stdio.h>
#include <pthread.h>

// The C library serializes the calls on a stream, so calls on stdout and
// stderr from two threads do not race on the streams' objects, nor on the
// buffer attached to stdout.
static char buf[BUFSIZ];

void *t_fun(void *arg) {
  fprintf(stdout, "a\n"); // NORACE
  printf("b\n"); // NORACE
  fputc('x', stderr); // NORACE
  return 0;
}

int main(void) {
  setvbuf(stdout, buf, _IOFBF, sizeof buf);
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  fprintf(stdout, "c\n"); // NORACE
  printf("d\n"); // NORACE
  fflush(stderr); // NORACE
  pthread_join(id, 0);
  return 0;
}
