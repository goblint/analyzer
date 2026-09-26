// PARAM: --set ana.activated[+] uninit
#include <stdlib.h>

// The operand of sizeof and __alignof__ is not evaluated, so nothing in it is read.
int main(void) {
  int *p;
  int *m = malloc(sizeof(*m)); // NOWARN
  size_t s1 = sizeof(*p); // NOWARN
  size_t s2 = __alignof__(*p); // NOWARN

  // The operand of sizeof is evaluated when its type is a variable-length array,
  // but the array itself is not read, only what its address is computed from.
  int n = rand() % 3 + 1;
  int vla[n];
  size_t s3 = sizeof(vla); // NOWARN
  int (*pv)[n];
  size_t s4 = sizeof(*pv); // WARN

  // A length that is the size of a variable-length array is not an integer constant expression,
  // so these are pointers to variable-length arrays too.
  typedef int V[n];
  int (*pv1)[sizeof vla];
  size_t s5 = sizeof(*pv1); // WARN
  int (*pv2)[sizeof(int[n])];
  size_t s6 = sizeof(*pv2); // WARN
  int (*pv3)[sizeof(V)];
  size_t s7 = sizeof(*pv3); // WARN
  int (*pv4)[sizeof(int[sizeof vla])];
  size_t s8 = sizeof(*pv4); // WARN

  // Alignments, and sizes of fixed-size arrays, are integer constant expressions.
  int (*pf1)[_Alignof(int[n])];
  size_t s9 = sizeof(*pf1); // NOWARN
  int (*pf2)[__alignof__(vla)];
  size_t s10 = sizeof(*pf2); // NOWARN
  int (*pf3)[sizeof(int[4])];
  size_t s11 = sizeof(*pf3); // NOWARN
  int (*pf4)[sizeof(int[sizeof(int)])];
  size_t s12 = sizeof(*pf4); // NOWARN
  return 0;
}
