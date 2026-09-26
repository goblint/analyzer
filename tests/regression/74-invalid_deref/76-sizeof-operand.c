// PARAM: --set ana.activated[+] useAfterFree --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <alloca.h>

// The operand of sizeof and __alignof__ is not evaluated, so nothing in it is dereferenced.
int main(void) {
  int *p;
  int *a = alloca(sizeof(*a)); // NOWARN
  int *m = malloc(sizeof(*m)); // NOWARN
  size_t s1 = sizeof(*p); // NOWARN
  size_t s2 = __alignof__(*p); // NOWARN

  int *q = malloc(sizeof(int));
  free(q);
  size_t s3 = sizeof(*q); // NOWARN
  size_t s4 = sizeof(q[3]); // NOWARN
  size_t s5 = __alignof__(*q); // NOWARN

  // The operand of sizeof is evaluated when its type is a variable-length array: vla[*q] reads *q.
  int n = rand() % 3 + 1;
  int vla[n][n];
  size_t s6 = sizeof(vla[*q]); // WARN

  // A length that is the size of a variable-length array is not an integer constant expression,
  // so w[*q] and w2[*q] are variable-length arrays and read *q.
  int w[1][sizeof vla];
  size_t s7 = sizeof(w[*q]); // WARN
  int w2[1][sizeof(int[n])];
  size_t s8 = sizeof(w2[*q]); // WARN
  return 0;
}
