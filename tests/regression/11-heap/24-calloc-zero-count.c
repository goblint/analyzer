// calloc(n, size) requests n * size bytes, so its size is zero when the
// count is zero, even if the element size is not. Under the default
// sem.malloc.zero either, calloc with a count that may be zero may return
// the null pointer; 24-calloc-zero-count.t checks that under
// sem.malloc.zero pointer the assumption libc.malloc-zero-returns-pointer is
// reported at that call, and nothing at calloc(4, sizeof(int)).
#include <stdlib.h>
#include <goblint.h>

int main(int argc, char **argv) {
  size_t n = argc - 1;
  int *a = calloc(n, sizeof(int));
  __goblint_check(a != 0); //UNKNOWN!
  int *b = calloc(4, sizeof(int));
  __goblint_check(b != 0);
  free(a);
  free(b);
  return 0;
}
