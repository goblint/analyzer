// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] mustlockHistory --set ana.activated[+] descendantLockset --set ana.activated[+] creationLockset --disable ana.thread.include-node
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id1, id2;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  global++; // RACE!
  return NULL;
}

void *t2(void *arg) {
  global++; // RACE!
  return NULL;
}

int main(void) {

  int maybe;
  if (maybe) {
    // fine
    pthread_mutex_lock(&mutex);

    pthread_create(&id1, NULL, t1, NULL);

    pthread_create(&id2, NULL, t2, NULL);
    pthread_join(id2, NULL);

  } else {
    // not fine    
    pthread_create(&id1, NULL, t1, NULL); // mutex is not locked :/
    
    pthread_mutex_lock(&mutex);
    
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join(id2, NULL);
  }
  
  pthread_mutex_unlock(&mutex);
  return 0;
}
