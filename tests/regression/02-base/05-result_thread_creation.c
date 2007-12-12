#include<stdio.h>
#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  printf("Not much to say when your high above the mucky-muck...\n");
  return NULL;
}

int glob1;
int glob2 = 9;

int main() {
  pthread_t id;

  if (pthread_create(&id, NULL, t_fun, NULL)) 
    exit(1);
  printf("Yeah! Yeah!\n");
  pthread_join(id, NULL);

  return 0;
}
