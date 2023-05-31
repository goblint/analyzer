#include <pthread.h>

int d;
pthread_t g;
enum { b } c() {}
void *e() { return &d; }
h() {}

void j() {
  int a;
  e();
  c();
  if (a)
    h();
}

void f() {
  pthread_create(&g, NULL, j, NULL);
}

void *i(void *arg) {
  f(g);
  return NULL;
}

void main() {
  pthread_t k;
  pthread_create(&k, NULL, i, NULL);
}