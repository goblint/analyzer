// PARAM: --set ana.path_sens[+] mutex
#include <assert.h>
#include <pthread.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

int main() {
  int r; // rand
  if (r) {
    r = 0;
    assert(1); // extra node in cfg
  }
  else {
    pthread_mutex_lock(&m);
    assert(1); // extra node in cfg
  }
  // TODO: even though we're path-sensitive w.r.t mutexes, there's only one path after join
  // because path set Hoare reduce is over all path partitions (ignores should_join)
  // and one path is leq of another, so Hoare maximality removes one
  return 0;
}
