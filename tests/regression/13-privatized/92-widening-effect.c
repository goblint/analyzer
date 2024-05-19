// PARAM: --set ana.base.privatization write --enable ana.int.interval
// For write side_widen never is needed, for protection it is not.
#include <pthread.h>
int occupied;
pthread_mutex_t mtx;


void* thread(void* arg) {
  pthread_mutex_lock(&mtx);
  if(occupied < 1) {
    occupied++;
  }
  pthread_mutex_unlock(&mtx);
 }

int main() {
  pthread_t worker;

  pthread_create(&worker, 0, &thread, 0);

  pthread_mutex_lock(&mtx);
  occupied = 0;
  pthread_mutex_unlock(&mtx);

  pthread_mutex_lock(&mtx);
  __goblint_check(occupied >= 0);
   pthread_mutex_lock(&mtx);
  return 0;
}
