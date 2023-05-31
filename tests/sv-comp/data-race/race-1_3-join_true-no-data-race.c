#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

extern void abort(void); 
extern void __VERIFIER_atomic_begin(void);
extern void __VERIFIER_atomic_end(void);
void reach_error(){}
int __VERIFIER_nondet_int(void);
void ldv_assert(int expression) { if (!expression) { ERROR: {reach_error();abort();}}; return; }

pthread_t t1;
pthread_mutex_t mutex;
int pdev;

void *thread1(void *arg) {
   pthread_mutex_lock(&mutex);
   __VERIFIER_atomic_begin();
   pdev = 6;
   __VERIFIER_atomic_end();
   pthread_mutex_unlock(&mutex);
   return 0;
}

int module_init() {
   pthread_mutex_init(&mutex, NULL);
   //not a race
   pdev = 1;
   ldv_assert(pdev==1);
   if(__VERIFIER_nondet_int()) {
      //enable thread 1
      pthread_create(&t1, NULL, thread1, NULL);
      //race
      //pdev = 2;
      //ldv_assert(pdev==2);
      return 0;
   }
   //not a race
   pdev = 3;
   ldv_assert(pdev==3);
   pthread_mutex_destroy(&mutex);
   return -1;
}

void module_exit() {
   void *status;
   //race
   __VERIFIER_atomic_begin();
   pdev = 4;
   __VERIFIER_atomic_end();
   __VERIFIER_atomic_begin();
   ldv_assert(pdev==4);
   __VERIFIER_atomic_end();
   pthread_join(t1, &status);
   pthread_mutex_destroy(&mutex);
   //not a race
   pdev = 5;
   ldv_assert(pdev==5);
}

int main(void) {
    if(module_init()!=0) goto module_exit;
    module_exit();
module_exit:
    return 0;
}
