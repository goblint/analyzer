// PARAM: --set ana.activated[+] pthreadOnce --enable ana.int.interval
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>
#include <stdlib.h>

struct {
  pthread_once_t once1;
  pthread_once_t once2;
} onces = {PTHREAD_ONCE_INIT, PTHREAD_ONCE_INIT};

int g;
pthread_mutex_t mtx;

void increment() {
  g++;
}

int main(void) {

  pthread_once(&onces.once1, increment);
  pthread_once(&onces.once2, increment);

  if(g == 2) {
    __goblint_check(1); // Reachable
  }
  printf("g = %d\n", g);

  return 0;
}
