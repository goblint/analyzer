//PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>

pthread_t mainid;

int g = 10;

void *t_fun(void *arg) {
  pthread_join(mainid, NULL);
  g++; // TODO NORACE
  return NULL;
}


int main(void) {
  mainid = pthread_self();

  pthread_t id2;
  pthread_create(&id2, NULL, t_fun, NULL);

  g++; // TODO NORACE
  return 0;
}
