// PARAM: --disable ana.thread.context.createEdges --set ana.activated[+] threadCreate
#include <goblint.h>
#include <pthread.h>

extern void unknown_fun(void *(*fun));

int glob;

void *t_FST(void *arg) {
}

void *t_SND(void *arg) {
  glob = 1; //RACE
}

int nothing () {
  unknown_fun(t_SND);
}


int main() {
  
  pthread_t id;
  pthread_create(&id, NULL, t_FST, NULL);

  nothing();

  glob = 2; //RACE

  pthread_t id;
  pthread_create(&id, NULL, t_SND, NULL);

  nothing();

}
