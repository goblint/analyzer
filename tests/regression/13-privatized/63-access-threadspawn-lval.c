#include <pthread.h>

pthread_t id1;
pthread_t id2;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void magic1();
void magic2();

void e() {
  pthread_mutex_lock(&A);
  magic1();
}

void *f(void *arg) {
  return NULL;
}

void *g(void *arg) {
  magic2(e); // spawns e
  return NULL;
}

void main() {
  // magic1(); // optional?
  pthread_create(&id1, NULL, f, NULL); // mutex should record id1 access here
  pthread_create(&id2, NULL, g, NULL); // mutex should record id2 access here
}
