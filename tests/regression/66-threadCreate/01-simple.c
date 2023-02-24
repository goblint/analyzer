// PARAM: --disable ana.thread.context.createEdges --set ana.activated[+] threadCreateEdges
#include <goblint.h>
#include <pthread.h>

int glob;

void *t_FST(void *arg) {
}

void *t_SND(void *arg) {
  glob = 1; //NORACE
}

int nothing () {
}


int main() {
  
  pthread_t id;
  pthread_create(&id, NULL, t_FST, NULL);

  nothing();

  glob = 2; //NORACE

  pthread_t id;
  pthread_create(&id, NULL, t_SND, NULL);

  nothing();

}
