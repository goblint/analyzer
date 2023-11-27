#include <pthread.h>
#include <stdlib.h>

int threads_total = 4;
pthread_t *tids;

void *cleaner(void *arg) {
  while (1) {
    for (int i = 0; i < threads_total; i++) {
      if (tids[i]) { // RACE!
        if (!pthread_join(tids[i], NULL)) // RACE!
          tids[i] = 0; // RACE!
      }
    }
  }
  return NULL;
}

void *thread(int i) { // wrong argument type is important
  tids[i] = pthread_self(); // RACE!
  return NULL;
}

int main() {
  pthread_t tid;
  tids = malloc(threads_total * sizeof(pthread_t));

  for(int i = 0; i < threads_total; i++)
    tids[i] = 0;

  pthread_create(&tid, NULL, cleaner, NULL);

  for(int i = 0; i < threads_total; i++) {
    pthread_create(&tid, NULL, thread, (int *)i); // cast is important
  }

  return 0;
}
