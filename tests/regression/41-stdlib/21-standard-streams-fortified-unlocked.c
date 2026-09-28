#include <stdio.h>
#include <pthread.h>

// glibc's aliased and fortified entry points for fread_unlocked take no lock
// on the stream, so each races on the buffer attached to the stream with a
// stdio call that takes the lock. Each pair has a stream and a buffer of its
// own.
extern size_t __fread_unlocked_alias(void *ptr, size_t size, size_t n, FILE *stream);
extern size_t __fread_unlocked_chk(void *ptr, size_t ptrlen, size_t size, size_t n, FILE *stream);
static char in[BUFSIZ], out[BUFSIZ];

void *t_fun(void *arg) {
  char c[8];
  __fread_unlocked_alias(c, 1, 1, stdin); // RACE
  __fread_unlocked_chk(c, sizeof c, 1, 1, stdout); // RACE
  return 0;
}

int main(void) {
  setvbuf(stdin, in, _IOFBF, sizeof in);
  setvbuf(stdout, out, _IOFBF, sizeof out);
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  fgetc(stdin); // RACE
  fputc('x', stdout); // RACE
  pthread_join(id, 0);
  return 0;
}
