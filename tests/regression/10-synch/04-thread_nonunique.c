// SKIP
#include <pthread.h>
#include <stdio.h>

int myglobal;

void *t_fun(void *arg) {
  myglobal=42; //RACE!
  return NULL;
}

int main(void) {
  pthread_t id[10];
  int i;
  for (i=0; i<10; i++)
    pthread_create(&id[i], NULL, t_fun, NULL);
  for (i=0; i<10; i++)
    pthread_join (id[i], NULL);
  return 0;
}
