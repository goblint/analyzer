// PARAM: --set ana.activated[+] pthreadOnce
// Developer used tow different pthread_once_t variables by mistake
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_once_t once1 = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void fun() {
  g++; //RACE!
  g++; //RACE!
}


void* thread(void* arg) {
  pthread_once(&once, fun);
  return NULL;
}

int main(void) {
  pthread_t id;

  pthread_create(&id, NULL, thread, NULL);
  pthread_once(&once1, fun);

  return 0;
}
