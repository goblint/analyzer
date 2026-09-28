#include <stdio.h>
#include <pthread.h>
#include <goblint.h>

// fflush(NULL) flushes every output stream, so it writes the buffers attached
// to stdout and stderr although its argument points to neither; so does a
// call whose argument may be null. A write of the program's own to one of
// those buffers races with it.
static char out[BUFSIZ];
static char err[BUFSIZ];
static char in[BUFSIZ];
FILE *maybe;

void *t_fun(void *arg) {
  fflush(NULL); // RACE
  fflush(maybe); // RACE
  return 0;
}

int main(int argc, char **argv) {
  setvbuf(stdout, out, _IOFBF, sizeof out);
  setvbuf(stderr, err, _IOFBF, sizeof err);
  setvbuf(stdin, in, _IOFBF, sizeof in);
  fputc('a', stdout);
  maybe = argc > 1 ? stdin : NULL;
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  out[0] = 'x'; // RACE
  err[0] = 'x'; // RACE
  pthread_join(id, 0);
  return 0;
}
