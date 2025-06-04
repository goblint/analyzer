// PARAM: --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void fun() {
  g++; //NORACE
  g++; //NORACE
}


void* thread(void* arg) {
  pthread_once(&once, fun);
  return NULL;
}

int main(void) {
  pthread_t id;

  pthread_create(&id, NULL, thread, NULL);
  pthread_once(&once, fun);

  return 0;
}
