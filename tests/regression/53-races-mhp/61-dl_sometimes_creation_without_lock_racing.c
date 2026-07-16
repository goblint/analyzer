// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  global++; // RACE!
  return NULL;
}

int main(void) {
  for (int i = 0; i < 3; i++) {
    if (i == 2) { 
      pthread_mutex_lock(&mutex);
    }
    pthread_create(&id1, NULL, t1, NULL); // Same TID created multiple times with different locksets
  }

  global++; // RACE!
  pthread_mutex_unlock(&mutex);
  return 0;
}
