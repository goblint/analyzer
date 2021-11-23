// PARAM: --disable sem.unknown_function.spawn --enable ana.int.interval
#include <stdlib.h>
#include <assert.h>

int cmp1(const void *a, const void *b) {
  assert(1); // reachable
  return 0;
}

int cmp2(const void *a, const void *b) {
  assert(a != NULL);
  assert(b != NULL);
  return 0;
}

int cmp3(const void *a, const void *b) {
  assert(1); // NOWARN (unreachable)
  return 0;
}

int cmp4(const void *a, const void *b) {
  int x = *((const int*) a);
  int y = *((const int*) b);
  assert(-3 <= x);
  assert(x <= 10);
  assert(-3 <= y);
  assert(y <= 10);
  return 0;
}

int main() {
  int arr1[] = {7, 5, 10, 8, 4, -3, 2};
  qsort(arr1, 7, sizeof(int), cmp1);

  int arr2[] = {7, 5, 10, 8, 4, -3, 2};
  qsort(arr2, 7, sizeof(int), cmp2);

  int arr3[] = {7, 5, 10, 8, 4, -3, 2};
  qsort(arr3, 0, sizeof(int), cmp3); // calling with empty

  int arr4[] = {7, 5, 10, 8, 4, -3, 2};
  qsort(arr4, 7, sizeof(int), cmp4);
  return 0;
}