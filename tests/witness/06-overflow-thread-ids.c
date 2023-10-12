// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *http_get(void *arg) { 
  short x = 7; 
  x = x + 2147483647;
}

int main() {
  int z = 4;
  pthread_t tid;
  pthread_create(&tid, NULL, &http_get, NULL);
  return 0;
}