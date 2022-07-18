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
  assert(x == 4);
  assert(-3 <= y);
  assert(y <= 10);
  return 0;
}

int cmp5(const void *a, const void *b) {
  int x = *((const int*) a);
  int y = *((const int*) b);
  int r = x - y;
  return r;
}

int main() {
  int arr[] = {-3, 2, 4, 5, 7, 8, 10};
  int key = 4;

  bsearch(&key, arr, 7, sizeof(int), cmp1);

  bsearch(&key, arr, 7, sizeof(int), cmp2);

  bsearch(&key, arr, 0, sizeof(int), cmp3); // calling with empty

  bsearch(&key, arr, 7, sizeof(int), cmp4);

  int *ret5 = bsearch(&key, arr, 7, sizeof(int), cmp5);

  // bsearch hasn't invalidated array
  for (int i = 0; i < 7; i++) {
    assert(-3 <= arr[i]);
    assert(arr[i] <= 10);
  }
  return 0;
}