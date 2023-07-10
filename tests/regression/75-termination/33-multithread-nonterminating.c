// NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

// Thread function
void *printPID(void *arg) {
  pid_t pid = getpid();
  pthread_t tid = pthread_self();
  while (1) {
    printf("Thread ID: %lu, Process ID: %d\n", (unsigned long)tid, pid);
    struct timespec sleepTime;
    sleepTime.tv_sec = 1; // Seconds
    sleepTime.tv_nsec =
        100000000 + (rand() % 200000000); // Nanoseconds (0.1 seconds + rand)
    printf("Sleep for %ld nsec\n", sleepTime.tv_nsec);
    nanosleep(&sleepTime, NULL);
  }
  return NULL;
}

int main() {
  // Create three threads
  pthread_t thread1, thread2, thread3;
  pthread_create(&thread1, NULL, printPID, NULL);
  pthread_create(&thread2, NULL, printPID, NULL);
  pthread_create(&thread3, NULL, printPID, NULL);

  // Wait for all threads to finish
  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);
  pthread_join(thread3, NULL);

  return 0;
}
