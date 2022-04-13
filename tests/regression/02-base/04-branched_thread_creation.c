#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1 = 3;
int glob2 = 9;

int main() {
  int k;
  int mt=0;
  pthread_t id;

  if (k) {
    mt=1;
    pthread_create(&id, NULL, t_fun, NULL);
  }
  else {
    glob1 = 4;
  }

  // We need to side-effect glob1=4 upon the join here;
  // otherwise, the global invariant will still have glob1=3.

  k = glob1;
  if(!mt) {
    // If we are not multi-threaded, or are not sure if we are
    if(k==3) {} else {
      // This must be reachable
      assert(1);
    }
  }

  k = glob2;
  assert(k == 9);

  return 0;
}
