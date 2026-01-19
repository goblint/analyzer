// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set "ana.activated[+]" creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  // everything from here must happen after unlock in main
  pthread_mutex_unlock(&mutex);
  global++; // RACE!
  return NULL;
}

void *t2(void *arg) { // t2 is joined into main before unlock happens
    global++; // RACE!
    return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);

  pthread_create(&id1, NULL, t1, NULL);

  pthread_create(&id2, NULL, t2, NULL);
  pthread_mutex_unlock(&mutex);
  pthread_join(id2, NULL);
  return 0;
}
