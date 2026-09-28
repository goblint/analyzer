// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr --set ana.activated[+] threadJoins
#include <stdio.h>
#include <pthread.h>

// A cancelled thread's frames end without a return or a pthread_exit of its
// own, once it acts on the cancellation, here at fputs.

static void *worker(void *arg) {
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // ASSUMED libc.stream-buffer-lifetime (the program calls pthread_cancel)
  while (1)
    fputs("a", stdout);
  return NULL;
}

int main(void) {
  pthread_t t;
  pthread_create(&t, NULL, worker, NULL);
  pthread_cancel(t); // WARN
  pthread_join(t, NULL);
  fflush(stdout);
  return 0;
}
