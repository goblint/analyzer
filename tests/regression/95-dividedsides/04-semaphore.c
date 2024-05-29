// PARAM: --enable solvers.td3.divided-narrow.enable --enable solvers.td3.divided-narrow.stable --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>
#include <unistd.h>

typedef struct {
    pthread_mutex_t mutex;
    int count;
} semaphore_t; 

void semaphor_init(semaphore_t *sem, int count) {
    sem->count = count;
    pthread_mutex_init(&sem->mutex, NULL);
}

void semaphor_up(semaphore_t *sem) {
    pthread_mutex_lock(&sem->mutex);
    if (sem->count < 1000)
        sem->count++;
    pthread_mutex_unlock(&sem->mutex);
}

void semaphor_down(semaphore_t *sem) {
    while(1) {
        pthread_mutex_lock(&sem->mutex);
        if(sem->count > 0) {
            sem->count--;
            pthread_mutex_unlock(&sem->mutex);
            break;
        }
        pthread_mutex_unlock(&sem->mutex);
        usleep(10);
    }
}

void worker(void *data) {
    semaphore_t* sem = (semaphore_t*)data;
    while(1) {
        semaphor_down(sem);
        // do work
        semaphor_up(sem);
    }
}

int main(void) {
  pthread_t id;
  semaphore_t sem;
  semaphor_init(&sem, 10);

  pthread_create(&id, NULL, worker, &sem);
  pthread_create(&id, NULL, worker, &sem);

  __goblint_check(sem.count <= 1000);
  pthread_mutex_lock(&sem.mutex);
  __goblint_check(sem.count <= 1000);
  pthread_mutex_unlock(&sem.mutex);

  __goblint_check(sem.count >= 0);
  pthread_mutex_lock(&sem.mutex);
  __goblint_check(sem.count >= 0);
  pthread_mutex_unlock(&sem.mutex);
  return 0;
}
