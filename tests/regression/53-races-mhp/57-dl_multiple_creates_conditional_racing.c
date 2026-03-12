// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --disable ana.thread.include-node
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2, id3;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  global++; // RACE!
  return NULL;
}

int main(void) {
  int maybe;
  if (maybe) {
    pthread_create(&id1, NULL, t1, NULL); // locking happens after thread creation!
    pthread_mutex_lock(&mutex);
  } else {
    pthread_mutex_lock(&mutex);
    pthread_create(&id2, NULL, t1, NULL);
  }
  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return 0;
}
