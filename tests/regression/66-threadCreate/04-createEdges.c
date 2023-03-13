// PARAM: --disable ana.thread.context.create-edges --set ana.activated[+] threadCreateEdges
#include <goblint.h>
#include <pthread.h>

int glob;

void *t_init(void *arg) {
}

void *t_norace(void *arg) {
  glob = 1; //NORACE
}

void *t_other(void *arg) {
}

int create_other () {
  pthread_t id;
  pthread_create(&id, NULL, t_other, NULL);
}


int main() {
  //enter multithreaded mode
  pthread_t id;
  pthread_create(&id, NULL, t_init, NULL);

  create_other();

  glob = 2; //NORACE

  pthread_t id;
  pthread_create(&id, NULL, t_norace, NULL);

  create_other();

}
