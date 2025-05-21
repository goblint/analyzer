// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12
#include<pthread.h>
#include<goblint.h>
void *a;

void* nothing(void* arg) {
  // Go multithreaded!
}

void main() {
  pthread_t t;
  pthread_create(&t, 0, nothing, 0);

  int d = 5;
  a = &d;

  if (0)
    ;

  __goblint_check(1); // Should be reachable!
}
