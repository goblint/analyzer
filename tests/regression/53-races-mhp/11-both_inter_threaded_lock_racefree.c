// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadDescendants --set ana.activated[+] creationLockset
#include <pthread.h>

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t id_main_child, id_1, id_1_child;

void *t1_child_fun(void *arg) { // t1child is protected by mutex locked in t1
  global++; // NORACE
  return NULL;
}

void *tmain_child_fun(void *arg) { // tmainchild is protected by mutex locked in main thread
  global++; // NORACE
  return NULL;
}

void *t1_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_create(&id_1_child, NULL, t1_child_fun, NULL);
  pthread_join(id_1_child, NULL);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_create(&id_1, NULL, t1_fun, NULL);
  pthread_mutex_lock(&mutex);
  pthread_create(&id_main_child, NULL, tmain_child_fun, NULL);
  pthread_join(id_main_child, NULL);
  pthread_mutex_unlock(&mutex);
  return 0;
}
