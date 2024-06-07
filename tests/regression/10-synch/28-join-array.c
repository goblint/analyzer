// PARAM: --set ana.activated[+] thread
#include <pthread.h>

int data = 0;
pthread_mutex_t data_mutex;

void *thread(void *arg) {
  pthread_mutex_lock(&data_mutex);
  data = 3; // RACE!
  pthread_mutex_unlock(&data_mutex);
  return NULL;
}

int main() {
  pthread_t tids[2];

  pthread_create(&tids[0], NULL, &thread, NULL);
  pthread_create(&tids[1], NULL, &thread, NULL);

  pthread_join(tids[0], NULL);

  data = 1; //RACE!

  return 1;
}
