// PARAM: --set ana.path_sens[+] mutex
#include <assert.h>
#include <pthread.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

int main() {
  int r; // rand
  if (r) {
    r = 0;
    assert(1); // extra node in cfg
    // r -> 0, lockset: {}
  }
  else {
    pthread_mutex_lock(&m);
    assert(1); // extra node in cfg
    // r -> 0, lockset: {m}
  }
  // OLD undesired behavior: one path: r -> 0, lockset: {}
  // NEW desired behavior: two paths: r -> 0, lockset: {} and r -> 0, lockset: {m}

  // OLD explanation:
  // even though we're path-sensitive w.r.t mutexes, there's only one path after join
  // because path set Hoare reduce is over all path partitions (ignores should_join)
  // and one path is leq of another, so Hoare maximality removes one

  // NEW explanation:
  // using SensitiveDomain correctly keeps both paths as path-sensitivity demands

  // TODO: manually check final join node in HTML
  // cannot be automated because concrete execution cannot check
  // if _must_ lockset _may_ contain m on some path
  return 0;
}
