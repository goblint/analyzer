#include <pthread.h>
#include <assert.h>

void *t_benign(void *arg) {
  return NULL;
}

int main() {
    int ret = 17;
    pthread_t id2;
    ret = pthread_create(&id2, NULL, t_benign, NULL);

    assert(ret == 17); //UNKNOWN!

    ret = 17;
    ret = pthread_join(id2,NULL);

    assert(ret == 17); //UNKNOWN!
    return 0;
}
