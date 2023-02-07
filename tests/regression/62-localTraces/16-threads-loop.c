// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *http_get(void *arg) {}

int main() {
  int z = 0;
  while (z < 7) {
    pthread_create(NULL, NULL, &http_get, NULL);
    z = z + 1;
  }
  return 0;
}