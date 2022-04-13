// PARAM: --set allfuns true
#include<pthread.h>
#include<assert.h>

int myglobal;
int three = 3;
pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void t1() {
  pthread_mutex_lock(&A_mutex);
  myglobal++; //RACE!
  pthread_mutex_unlock(&A_mutex);

  assert(three == 3);
}

void t2() {
  pthread_mutex_lock(&B_mutex);
  myglobal++; //RACE!
  pthread_mutex_unlock(&B_mutex);

  assert(three == 3);
}
