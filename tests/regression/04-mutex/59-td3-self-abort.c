#include <pthread.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=1; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  myglobal = myglobal+1; // RACE!
  return 0;
}
