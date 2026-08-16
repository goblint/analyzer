// SKIP PARAM: --set ana.activated '["gStoreWidening","effectivelyLocal","assert","base","mallocWrapper","thread","threadid","escape"]' --set ana.base.privatization none --enable exp.globs_are_top
// Additional analyses are activated so framework can handle thread creation
#include <goblint.h>
#include <pthread.h>
int global = 0;
int thread_owned = 0;

void* thread(void* arg) {

  thread_owned = 42;
  __goblint_check(thread_owned == 42);

  if(global < 0) {
     global = -58;
  } else {
    global = 1;
  }

  thread_owned = 11;
  __goblint_check(thread_owned == 11);

  return NULL;
}

int main() {
  int x = 11;
  global = 0;

  pthread_t t;
  pthread_create(&t, NULL, &thread, NULL);

  global = x*x;

  if(global > 200) {
    global = -12;
  }

  __goblint_check(global < 200);
  __goblint_check(global >= 0);
  __goblint_check(thread_owned >= 0);


  pthread_join(t, NULL);

  global = 42;

  // This is out of reach here
  __goblint_check(global == 42);

  return 0;
}
