// PARAM: --enable solvers.td3.narrow-globs.enabled --enable ana.int.interval --enable ana.base.priv.protection.changes-only
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
  semaphore_t sem;
  semaphor_init(&sem, 10);

  pthread_t id1;
  pthread_create(&id1, NULL, worker, &sem);
  pthread_t id2;
  pthread_create(&id2, NULL, worker, &sem);

  __goblint_check(sem.count <= 1000);
  pthread_mutex_lock(&sem.mutex);
  __goblint_check(sem.count <= 1000);
  pthread_mutex_unlock(&sem.mutex);

  __goblint_check(sem.count >= 0);
  pthread_mutex_lock(&sem.mutex);
  __goblint_check(sem.count >= 0);
  pthread_mutex_unlock(&sem.mutex);

  pthread_join(&id1, NULL);
  pthread_join(&id2, NULL);
  return 0;
}
