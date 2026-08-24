// PARAM: --set ana.activated[+] pthreadOnce
// Function to be called by once is passed as a pointer
#include <pthread.h>
#include <stdio.h>

int g;
void init0();

void* initp = &init0;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void init0() {
  g++; //NORACE
}


void init1() {
  g++; //NORACE
}


void* thread(void* arg) {
  pthread_once(&once, initp);
  return NULL;
}

int main(void) {
  pthread_t id;
  int top;

  pthread_create(&id, NULL, thread, NULL);
  if(top) { initp = &init1; }
  pthread_once(&once, initp);


  return 0;
}
