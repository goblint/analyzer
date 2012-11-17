// PARAM: --set allfuns true
#include<pthread.h>

int myglobal;
pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void t1() {
  pthread_mutex_lock(&A_mutex);
  myglobal++; //RACE!
  pthread_mutex_lock(&A_mutex);
}

void t2() {
  pthread_mutex_lock(&B_mutex);
  myglobal++; //RACE!
  pthread_mutex_lock(&B_mutex);
}
