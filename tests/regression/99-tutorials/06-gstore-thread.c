// PARAM: --set ana.activated '["gStoreWidening","assert","base","mallocWrapper"]' --set ana.base.privatization none --enable exp.globs_are_top
// Additional analyses are activated so framework can handle thread creation
#include <goblint.h>
#include <pthread.h>
int global = 0;

void* thread(void* arg) {

  if(global < 0) {
    global = -58;
  } else {
    global = 1;
  }

  return NULL;
}

int main() {
  int x = 11;

  pthread_t t;
  pthread_create(&t, NULL, &thread, NULL);

  global = x*x;

  if(global > 200) {
    global = -12;
  }

  __goblint_check(global < 200);
  __goblint_check(global >= 0);


  pthread_join(t, NULL);

  global = 42;

  // This is out of reach here
  __goblint_check(global == 42);

  return 0;
}
