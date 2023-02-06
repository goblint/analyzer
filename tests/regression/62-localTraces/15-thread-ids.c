// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *http_get(void *arg) { int x = 7; }

int main() {
  int z = 4;
  pthread_t tid;
  int y = pthread_create(&tid, NULL, &http_get, NULL);
  return 0;
}