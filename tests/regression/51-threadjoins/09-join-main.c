//PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>
#include <stdio.h>

pthread_t mainid;

int g = 10;

void *t_fun(void *arg) {
  int r = pthread_join(mainid, NULL); // TSan doesn't like this...
  printf("j: %d\n", r);
  g++; // NORACE
  printf("t_fun: %d\n", g);
  return NULL;
}


int main(void) {
  mainid = pthread_self();

  pthread_t id2;
  pthread_create(&id2, NULL, t_fun, NULL);

  g++; // NORACE
  printf("main: %d\n", g);

  pthread_exit(NULL); // exit main thread but keep id2 alive, otherwise main returning kills id2
  return 0;
}
