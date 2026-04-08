// PARAM: --set ana.context.gas_value 0 --set ana.activated[+] thread --set ana.activated[+] threadid

#include <pthread.h>
#include <stdio.h>

int myglobal;
int myglobal2;

void *t_flurb(void *arg) {
  myglobal=40; //RACE
  return NULL;
}

void* bla(void *arg) {
    return NULL;
}

void *t_fun(void *arg) {
  unknown(t_flurb); // NOCRASH
  return NULL;
}

int main(void) {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL);
    pthread_create(&id, NULL, t_fun, NULL);

    unknown(bla);

    return 0;
}
