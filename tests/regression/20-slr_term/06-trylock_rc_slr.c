// PARAM: --enable ana.int.interval --sets solver slr3t
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
long counter = 0;

void *counter_thread (void *arg) {
    int tmp = counter;
    int i = 0;
    pthread_mutex_lock (&mutex);
    while(i<5){
      tmp = counter;
      tmp++;
      counter = tmp;
      i++;
    }
    pthread_mutex_unlock (&mutex);
    tmp = 1;
}

int main (int argc, char *argv[]) {
  pthread_t counter_thread_id;
  pthread_create (&counter_thread_id, NULL, counter_thread, NULL);
  return 0;
}
