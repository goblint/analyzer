// PARAM: --disable ana.thread.context.createEdges --set ana.activated[+] threadCreateEdges
#include <goblint.h>
#include <pthread.h>

int glob_noCreate;
int glob_create;

void *t_INIT(void *arg) {
}

void *t_noCreate(void *arg) {
  glob_noCreate =1; //NORACE
}

void *t_create(void *arg) {
  glob_create =1; //RACE
}

void noCreate1 () {
  noCreate2();
}
void noCreate2 () {
  noCreate3();
}
void noCreate3 () {
  noCreate4();
}
void noCreate4 () {
  noCreate5();
}
void noCreate5 () {
}

void create1 () {
  create2();
}
void create2 () {
  create3();
}
void create3 () {
  create4();
}
void create4 () {
  create5();
}
void create5 () {
  pthread_t id;
  pthread_create(&id, NULL, t_create, NULL);
}

int main() {
  
  pthread_t id;
  pthread_create(&id, NULL, t_INIT, NULL);

  //no create
  noCreate1();

  glob_noCreate = 2; //NORACE

  pthread_t id;
  pthread_create(&id, NULL, t_noCreate, NULL);

  noCreate1();

  //create
  create1();

  glob_create = 2; //RACE

  pthread_t id;
  pthread_create(&id, NULL, t_create, NULL);

  create1();

}
