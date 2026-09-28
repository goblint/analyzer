// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr --set ana.activated[+] threadJoins
#include <stdio.h>
#include <pthread.h>

// The correct patterns at pthread_exit: a static buffer, which outlives every
// thread, and a stream closed before the exiting thread's frames end.
static char static_buffer[BUFSIZ];

static void *worker(void *arg) {
  setvbuf(stderr, static_buffer, _IOFBF, sizeof static_buffer);
  fputs("a", stderr);
  pthread_exit(NULL); // NOWARN
}

int main(void) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer);
  printf("b");
  fclose(stdout);
  pthread_t t;
  pthread_create(&t, NULL, worker, NULL);
  pthread_join(t, NULL);
  pthread_exit(NULL); // NOWARN
}
