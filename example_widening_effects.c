#include <pthread.h>
struct s {
  int occupied;
  int closed;
};

struct s pqb;
pthread_mutex_t mtx;

void pqueue_close() {
  pthread_mutex_lock(&mtx);
  pqb.closed = 1;
  pthread_mutex_unlock(&mtx);
}

void* thread(void* arg) {
  pthread_mutex_lock(&mtx);

  if(pqb.occupied < 2) {
    pqb.occupied++;
  }
  pthread_mutex_unlock(&mtx);
 }

int main() {
  pthread_t worker;

  pthread_create(&worker, 0, &thread, 0);
  pqueue_close();
  return 0;
}
