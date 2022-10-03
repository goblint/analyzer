#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  // awkward formatting to check that warning is just on condition expression, not entire if
  if // NORACE
    (myglobal > 0) { // RACE!
    printf("Stupid!");
    printf("Stupid!");
    printf("Stupid!");
    printf("Stupid!");
    printf("Stupid!");
    printf("Stupid!");
  }
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  myglobal=myglobal+1; // RACE!
  pthread_join (id, NULL);
  return 0;
}
