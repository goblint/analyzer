#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1 = 3;
int glob2 = 9;

int main() {
  int k;
  int mt = 0;
  pthread_t id;

  if (k) {
    mt = 1;
    pthread_create(&id, NULL, t_fun, NULL);
  } else {
    glob1 = 4;
  }


  // We need to side-effect glob1=4 upon the join here;
  // otherwise, the global invariant will still have glob1=3.

  k = glob1;
  if(!mt) {
    // If we are not multi-threaded, or are not sure if we are
    if(k==3) {} else {
      // This must be reachable
      __goblint_check(1);
    }
  }

  k = glob2;
  __goblint_check(k == 9);

  // This would cause glob1=4 side effect to disappear
  // if it's side-effected only on return.
  glob1 = 3;

  return 0;
}
