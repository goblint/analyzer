// PARAM: --set ana.activated[+] pthreadOnce --enable ana.int.interval
#include <pthread.h>
#include <stdio.h>
// #include <goblint.h>
#include <stdlib.h>

pthread_once_t once1;
int g = 0;

void increment() {
  g++;
}

int main(void) {
  pthread_once(&once1, increment);

  if(once1 != 0) {
    __goblint_check(1); // Reachable
  }

  // On GCC prints 2
  printf("once1 = %d\n", once1);

  return 0;
}
