// PARAM: --set pre.cppflags[+] "-DGOBLINT_NO_PTHREAD_ONCE" --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void fun() {
  g++;
}


int main(void) {
  pthread_t id;

  pthread_once(&once, fun);
  pthread_once(&once, fun);

  // This fails if the (actual) case that it is not executed twice is ignored.
  __goblint_check(g == 1); //TODO

  return 0;
}
