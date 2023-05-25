// PARAMS: --disable exp.volatiles_are_top
#include <pthread.h>
#include <goblint.h>
#include <setjmp.h>

jmp_buf buf;
jmp_buf buf1;
pthread_mutex_t m;
int g;

void blub(int *x) {
  *x = 17;
}

void blarg() {
  int x = 28;
  blub(&x);
}

void *t_benign(void *arg) {
  volatile int vol = 2;
  int t = 42, top;

  if(setjmp(buf1)) {
    t = t+1; //WARN
    return NULL;
  }

  t = 19;

  if(setjmp(buf)) {
    t = t+1; //NOWARN
    return NULL;
  }

  blarg();
  vol++;
  g = 17;

  if(top) {
      longjmp(buf1, 1); //WARN
  } else{
      longjmp(buf, 1); //NOWARN
  }
}


int main(void) {
  int t;

  pthread_t id;
  pthread_create(&id, NULL, t_benign, NULL);

  return 0;
}
