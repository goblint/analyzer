// PARAM: --enable ana.int.interval --sets ana.base.privatization mutex-meet --disable sem.unknown_function.invalidate.globals --disable sem.unknown_function.spawn
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <signal.h>
#include <assert.h>

pthread_mutex_t mt;
int i = 0;

void* fn1(void* agr)
{
  int top = rand();

  pthread_mutex_lock(&mt);
  if(top) {
    i = 5;
  }
  pthread_mutex_lock(&mt);
  assert(i == 0); //UNKNOWN!
  i = 0;
  pthread_mutex_unlock(&mt);
  pthread_mutex_unlock(&mt);
}

void* fn2(void* agr)
{
  int top = rand();

  pthread_mutex_lock(&mt);
  if(top) {
    i = 5;
  }
  top = pthread_mutex_lock(&mt);
  assert(i == 0); //UNKNOWN!
  i = 0;
  pthread_mutex_unlock(&mt);
  pthread_mutex_unlock(&mt);
}


int main()
{
  pthread_t tid1;
  pthread_t tid2;

  pthread_mutexattr_t mat;
  pthread_mutexattr_init(&mat);

  //The type of lock set is recursive
  pthread_mutexattr_settype(&mat, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&mt, &mat);

  pthread_create(&tid1, NULL, fn1, NULL);
  pthread_create(&tid2, NULL, fn2, NULL);

  pthread_join(tid1,NULL);
}
