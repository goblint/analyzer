// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set ana.activated[+] creationLockset --disable ana.thread.include-node
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  // everything from here must happen after unlock in main
  pthread_mutex_unlock(&mutex);
  global++; // NORACE
  return NULL;
}

void *t2(void *arg) { // t2 is joined into main before unlock happens
  global++; // NORACE
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);

  int maybe;
  if (maybe) {
    pthread_create(&id1, NULL, t1, NULL);

    pthread_create(&id2, NULL, t2, NULL);
    pthread_join(id2, NULL);
  } else {
    // this does not break the hb-relationship
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join(id2, NULL);

    pthread_create(&id1, NULL, t1, NULL);
  }
  
  pthread_mutex_unlock(&mutex);
  return 0;
}
