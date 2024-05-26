// PARAM: --enable solvers.td3.divided-narrow.enable --enable solvers.td3.divided-narrow.stable --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>
#include <unistd.h>

// TODO: again, this does not work due to privatization.
// As established in 03, protected values can typically not be narrowed.
// In this example, there is one guarded increment and one
// guarded decrement. Widening on the side of increment to the protected sem->count
// leads to [min, 10]. The protected sem->count influences the value arriving at the
// increment. The increment leads to side [min + 1, 1000] to the unprotected
// sem->count. Conversely, the increment produces side [0, max] to protected sem->count,
// so the decrement produces [0, min - 1] to unprotected sem->count.
// At the end, nearly nothing is known about both protected and unprotected sem->count.

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

  __goblint_check(sem.count >= 0); // UNKNOWN due to privatization issue (TODO)
  pthread_mutex_lock(&sem.mutex);
  __goblint_check(sem.count >= 0); // UNKNOWN due to privatization issue (TODO)
  pthread_mutex_unlock(&sem.mutex);
  return 0;
}
