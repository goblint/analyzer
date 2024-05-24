// SKIP PARAM: --set ana.activated[+] apron  --set ana.relation.privatization mutex-meet --sets ana.apron.domain interval
// Checks that branching over extern or volatile variables does not yield to both branches being dead.
// This fails unless exp.hide-std-globals is deactivated (which it is by default on this branch)
#include<pthread.h>
extern int optind;

void* a(void* arg) {
  // Just go multi-threaded
}

void main() {
  pthread_t t;

  pthread_create(&t, 0, a, 0);
  if (optind)
    ;

  // __goblint_check(1); // Reachable
}
