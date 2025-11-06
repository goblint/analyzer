// PARAM: --set ana.activated[+] pthreadOnce --set ana.activated[+] threadJoins --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid
// pthread_once object is passed as a pointer (and it may change)
#include <pthread.h>
#include <stdio.h>

int* g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mtx;

void init0() {
  g = malloc(sizeof(int));
}


void* thread(void* arg) {
  pthread_once(&once, init0);

  pthread_mutex_lock(&mtx);
  if(g == NULL) {
    // Will not happen!
    // Also showing that g is not NULL here requires "Extended Support for pthread once" as explained in appendix C.
    // It was added manually here so we can show off that at the end we can exclude the in initial value even
    // though main didn't write
    exit(42);
  }
  *g = 4711; //NORACE
  pthread_mutex_unlock(&mtx);

  return NULL;
}

int main(void) {
  pthread_t id, id2;
  int top;

  pthread_create(&id, NULL, thread, NULL);
  pthread_create(&id2, NULL, thread, NULL);

  pthread_join(id, NULL);
  pthread_join(id2, NULL);

  *g = 47; //NORACE

  return 0;
}
