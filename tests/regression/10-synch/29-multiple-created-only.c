// PARAM: --set ana.activated[+] thread --set ana.activated[+] threadid --set ana.thread.domain plain

#include <pthread.h>
#include <stdio.h>

int myglobal;
int myglobal2;

void* bla(void *arg) {
  // This is created multiple times, it should race with itself
  myglobal = 10; //RACE
  return NULL;
}

void* other(void) {
  // This is created only once, it should not be marked as non-unique
  unknown(bla);
  myglobal2 = 30; //NORACE
}

int main(void) {
    pthread_t id;
    pthread_create(&id, NULL, other, NULL);

    return 0;
}
