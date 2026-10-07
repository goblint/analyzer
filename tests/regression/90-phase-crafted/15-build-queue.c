// CRAM
// Build queue tracks compile/package/test phases with array-like build recipe loops.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t build_lock;
struct Build { int compiled; int packaged; int tested; } build;

static int recipe_cost(const int *steps, int n) {
  int cost = 0;
  for (int i = 0; i < n; i++)
    cost += steps[i];
  return cost;
}

void *compiler(void *arg) {
  int steps[] = {1, 2, 3};
  pthread_mutex_lock(&build_lock);
  /* GHOST compiler 1 */ build.compiled += recipe_cost(steps, 3);
  pthread_mutex_unlock(&build_lock);
  return 0;
}

void *packager(void *arg) {
  int boxes = 1;
  for (int i = 0; i < 3; i++)
    boxes += (i != 1);
  pthread_mutex_lock(&build_lock);
  /* GHOST packager 1 */ build.packaged += boxes;
  pthread_mutex_unlock(&build_lock);
  pthread_mutex_lock(&build_lock);
  /* GHOST packager 2 */ build.compiled -= 1;
  pthread_mutex_unlock(&build_lock);
  return 0;
}

void *tester(void *arg) {
  int passed = 0;
  for (int i = 0; i < 4; i++)
    passed += (i < 2);
  pthread_mutex_lock(&build_lock);
  /* GHOST tester 1 */ build.tested += passed;
  pthread_mutex_unlock(&build_lock);
  return 0;
}

int main(void) {
  pthread_t c, p, t;
  pthread_mutex_init(&build_lock, 0);
  build.compiled = 4;
  build.packaged = 2;
  build.tested = 1;
  pthread_create(&c, 0, compiler, 0);
  pthread_create(&p, 0, packager, 0);
  pthread_create(&t, 0, tester, 0);
  pthread_join(c, 0);
  pthread_join(p, 0);
  pthread_join(t, 0);
  if (build.packaged != 5 || build.tested != 3) {
    reach_error();
    abort();
  }
  return 0;
}
