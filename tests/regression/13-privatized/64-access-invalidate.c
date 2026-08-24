// CRAM
#include <pthread.h>

pthread_t id;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void magic1();
void magic2();

void e() {
  pthread_mutex_lock(&A);
  magic1();
}

void *g(void *arg) {
  magic2(e); // spawns e, mutex should record invalidated A access here
  return NULL;
}

void main() {
  pthread_create(&id, NULL, g, NULL); // mutex should record id access here
}
