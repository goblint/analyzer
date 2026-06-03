// PARAM: --set sem.lock.fail true
/* Based on trylock.c in Butenof's Programming with Posix C. */
#include <pthread.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define SPIN 10000000

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
long counter;
time_t end_time;

void *monitor_thread (void *arg) {
  int status;
  int misses = 0;

  while (time (NULL) < end_time) {
    sleep (3);
    status = pthread_mutex_trylock (&mutex); // ghost_1 = 1
    if (status != EBUSY) {
      status = pthread_mutex_unlock (&mutex); // ghost_1 = 0
    } else
      misses++;
  }
  printf ("Monitor thread missed update %d times.\n", misses);
  return NULL;
}

int main (int argc, char *argv[]) {
  int status;
  pthread_t monitor_thread_id;

  status = pthread_create (&monitor_thread_id, NULL, monitor_thread, NULL);
  if (status != 0)
    err_abort (status, "Create monitor thread");
  status = pthread_join (monitor_thread_id, NULL);
  if (status != 0)
    err_abort (status, "Join monitor thread");
  return 0;
}
