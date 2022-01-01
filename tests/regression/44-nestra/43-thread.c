//PARAM: --enable ana.int.interval
#include <pthread.h>

extern int printf();

void *sumP (void *x) {
  int k = 10;
  int sum, i;
  i = 0;
  while (i < k) {
    i++;
    sum += i;
  }
  assert(i == 10);
  printf("%d\n", sum);
}

void *prodP (void *x) {
  int k = 10;
  int prod, i;
  i = 0;
  while (i < k) {
    i++;
    prod *= i;
  }
  assert(i == 10);
  printf("%d\n", prod);
}

main() {
  int k;
  pthread_t sumT, prodT;
  pthread_create(&sumT, NULL, sumP, NULL);
  pthread_create(&prodT, NULL, prodP, NULL);
  return 0;
}
