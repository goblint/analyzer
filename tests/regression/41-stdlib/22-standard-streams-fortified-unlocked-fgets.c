#include <stdio.h>
#include <pthread.h>

// 21-standard-streams-fortified-unlocked.c for fgets_unlocked's aliased entry
// point, which takes no lock on the stream either.
extern char *__fgets_unlocked_alias(char *s, int n, FILE *stream);
static char err[BUFSIZ];

void *t_fun(void *arg) {
  char c[8];
  __fgets_unlocked_alias(c, sizeof c, stderr); // RACE
  return 0;
}

int main(void) {
  setvbuf(stderr, err, _IOFBF, sizeof err);
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  fputs("x", stderr); // RACE
  pthread_join(id, 0);
  return 0;
}
