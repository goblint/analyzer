// SKIP This is not testing anything right now...
#include<stdlib.h>
#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1;
int glob2 = 9;

int main() {
  pthread_t id;

  if (pthread_create(&id, NULL, t_fun, NULL)) 
    exit(1);
  pthread_join(id, NULL);

  return 0;
}
