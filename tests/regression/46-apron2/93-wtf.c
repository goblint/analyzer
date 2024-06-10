// SKIP PARAM: --set ana.activated[+] apron  --set ana.relation.privatization mutex-meet --sets ana.apron.domain interval
// Checks that branching over extern or volatile variables does not yield to both branches being dead.
#include<pthread.h>
#include<goblint.h>
extern int d;
volatile int v;

void* g(void* arg) {
  // Just go multithreaded!
}

void main() {
  pthread_t t;
  pthread_create(&t, 0, g, 0);

  if(v)
  ;

  __goblint_check(1); // Reachable

  if (d)
  ;

  __goblint_check(1); // Reachable

}
