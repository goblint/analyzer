// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *http_get(void *arg) {}

int main() {
  int z = 0;
  pthread_t x;
  while (z < 7) {
    pthread_create(&x, NULL, &http_get, NULL);
    z = z + 1;
  }
  return 0;
}