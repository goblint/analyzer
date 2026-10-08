// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info --disable warn.race
// A standard stream modified by another thread is checked like any other pointer.
#include <stdio.h>
#include <pthread.h>

char small[2];

void *t(void *arg) {
  stdout = (FILE *)(small + 5);
  return NULL;
}

int main() {
  pthread_t id;
  fflush(stdout); // NOWARN
  pthread_create(&id, NULL, t, NULL);
  fflush(stdout); // WARN
  fflush(stderr); // NOWARN
  return 0;
}
