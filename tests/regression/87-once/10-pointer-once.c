// PARAM: --set ana.activated[+] pthreadOnce
// pthread_once object is passed as a pointer (and it may change)
#include <pthread.h>
#include <stdio.h>

int g;
void init0();

pthread_once_t* optr;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_once_t once1 = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void init0() {
  g++; //RACE
}


void* thread(void* arg) {
  pthread_once(&once, init0);
  return NULL;
}

int main(void) {
  pthread_t id;
  int top;

  pthread_create(&id, NULL, thread, NULL);
  if(top) { optr = &once1; }
  pthread_once(optr, init0);

  return 0;
}
