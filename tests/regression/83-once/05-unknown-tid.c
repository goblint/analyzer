// PARAM: --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void fun() {
  g++; //NORACE
}


void* thread(void* arg) {
  pthread_once(&once, fun);
  return NULL;
}

int main(void) {
  pthread_t id;

  for(int i=0; i < 100; i++) {
    // Will receive unknown TID
    pthread_create(&id, NULL, thread, NULL);
  }

  return 0;
}
