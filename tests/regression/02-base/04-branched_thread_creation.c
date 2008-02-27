#include<stdio.h>
#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  printf("Not much to say when your high above the mucky-muck...\n");
  return NULL;
}

int glob1 = 3;
int glob2 = 9;

int main() {
  int k;
  pthread_t id;

  if (k) pthread_create(&id, NULL, t_fun, NULL);
  else glob1 = 4;
  printf("Yeah! Yeah!\n");
  pthread_join(id, NULL);
  assert(glob1 == 3); // UNKNOWN
  assert(glob2 == 9);

  return 0;
}
