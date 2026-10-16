// SKIP CRAM PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
// SKIP and CRAM because cannot test for specific kinds of warning
// disable fortification (__builtin___memset_chk) because cram test warnings would be different
#define _FORTIFY_SOURCE 0
#include <stdlib.h>
#include <string.h>

void issue_1801() {
  int **buffer;
  buffer = malloc(5 * sizeof(int *));

  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      int x = i * 5 + j;
      buffer[i][j] = x; // NOWARN (OOB)
    }
  }
}

void count_add() {
  char *p = malloc(42);
  size_t r;
  memset(p + 1, 0, r); // NOWARN (OOB)
}

void size_mul() {
  long long arr[4611686018427387903L]; // 64bit OCaml max_int (2^62-1) because CIL array length is int (not Z.t)
  arr[0] = 42; // NOWARN (overflow)
  arr[-1] = 42; // TODO WARN! (OOB)
  *(arr - 1) = 42; // TODO WARN! (OOB)

  long long *parr = &arr;
  *parr = 42; // NOWARN
  *(parr - 1) = 42; // WARN! (OOB)
}

int main() {
  issue_1801();
  count_add();
  size_mul();
  return 0;
}
