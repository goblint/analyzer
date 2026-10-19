// PARAM: --set sem.malloc.zero pointer
// With sem.malloc.zero pointer, an allocation whose size may be zero returns
// a pointer that is not null, and the analysis reports the assumption
// libc.malloc-zero-returns-pointer at each such call; 23-malloc-zero-pointer.t
// checks the report. An allocation of a size shown nonzero needs no
// assumption.
#include <stdlib.h>
#include <goblint.h>

int main(int argc, char **argv) {
  size_t n = argc;
  int *a = malloc(n * sizeof(int)); // assumption reported here
  __goblint_check(a != 0);
  int *b = malloc(sizeof(int)); // no assumption: the size is 4
  __goblint_check(b != 0);
  int *c = realloc(b, n); // assumption reported here
  __goblint_check(c != 0);
  free(a);
  free(c);
  return 0;
}
