// PARAM: --enable ana.int.interval --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.activated[+] threadJoinsPool --set lib.activated[+] klever
#include <goblint.h>
#include <pthread.h>

int data;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg) {
  pthread_mutex_lock(&mutex);
  data++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;

  pthread_create_N(&id, NULL, worker, NULL);
  pthread_join(id, NULL); // one handle cannot cover all dynamic instances

  data++; // RACE!
  return 0;
}
