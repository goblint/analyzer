// PARAM: --enable ana.int.interval --set ana.int.refinement once
#include <assert.h>

int main();

int withint() {
  int i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}

int withuint() {
  unsigned int i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}

int withlong() {
  long i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}

int withlonglong() {
  long long i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}

int withulonglong() {
  unsigned long long i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}

int main() {
  withint();
  withuint();
  withlong();
  withlonglong();
  withulonglong();

  unsigned long i = 0;
  void* bla;

  while(i < 10000) {
    i++;
    bla = &main;
  }

  assert(1); // reachable
  return 0;
}
