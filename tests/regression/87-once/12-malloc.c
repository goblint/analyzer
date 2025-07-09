// PARAM: --set ana.activated[+] pthreadOnce --enable ana.int.interval
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>
#include <stdlib.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mtx;

void increment() {
  g++;
}

int main(void) {
  pthread_once_t* optr5;
  pthread_once_t* optr;

  for(int i = 0; i < 10; i++) {
    optr = malloc(sizeof(pthread_once_t));
    *optr = PTHREAD_ONCE_INIT;

    if(i == 5) {
      optr5 = optr;
    }
  }

  pthread_once(optr, increment);
  pthread_once(optr5, increment);

  if(g == 2) {
    __goblint_check(1); // Reachable
  }


  return 0;
}
