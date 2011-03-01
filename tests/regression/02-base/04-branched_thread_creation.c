#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1 = 3;
int glob2 = 9;

int main() {
  int k;
  pthread_t id;

  if (k)
    pthread_create(&id, NULL, t_fun, NULL);
  else
    glob1 = 4;

  // We need to side-effect glob1=4 upon the join here;
  // otherwise, the global invariant will still have glob1=3.

  k = glob1;
  assert(k == 3); // UNKNOWN

  k = glob2;
  assert(k == 9);

  return 0;
}

