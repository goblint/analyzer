// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <pthread.h>

jmp_buf env_buffer;
int global = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  global = 3; // NORACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int bar() {
   pthread_mutex_lock(&mutex1);
   if(global ==3) {
      longjmp(env_buffer, 2);
   }
   return 8;
}

int main() {
   pthread_t id;
   pthread_create(&id, NULL, t_fun, NULL);

   if(!setjmp( env_buffer )) {
      bar();
   }

   global = 5; // NORACE
   pthread_mutex_unlock(&mutex1);
}
