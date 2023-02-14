#include <pthread.h>
#include <goblint.h>
#include <setjmp.h>

jmp_buf buf;
jmp_buf buf1;

void *t_benign(void *arg) {
    if(setjmp(buf1)) {
        return NULL;
    }

    longjmp(buf1, 1); //NOWARN
}

void *t_fun(void *arg) {
    if(setjmp(buf)) {
        return NULL;
    }

    longjmp(buf, 1); //WARN
}

int main(void) {
  int t;

  pthread_t id;
  pthread_create(&id, NULL, t_benign, NULL);

  pthread_t id2[10];
  for(int i =0; i < 10;i++) {
    // As we have both a unique & a non-unique thread here, we automatically warn as appropriate
    pthread_create(&id2[i], NULL, t_fun, NULL);
  }

  return 0;
}
