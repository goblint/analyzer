// PARAM: --set ana.thread.domain plain --set ana.thread.force-unique[+] t_fun
#include <pthread.h>
#include <stdio.h>

int myglobal[10];

void *t_fun(void *arg) {
  int i = (int) arg;
  myglobal[i]=42; // NORACE!
  return NULL;
}

int main(void) {
  pthread_t id[10];
  int i;
  for (i=0; i<10; i++)
    pthread_create(&id[i], NULL, t_fun, (void *) i);
  for (i=0; i<10; i++)
    pthread_join (id[i], NULL);
  return 0;
}
