With sem.malloc.zero pointer, calloc with a count the analysis cannot show
nonzero reports libc.malloc-zero-returns-pointer; calloc(4, sizeof(int))
reports nothing.

  $ goblint --set sem.malloc.zero pointer 24-calloc-zero-count.c 2>&1 | grep Assumption
  [Info][Assumption] libc.malloc-zero-returns-pointer: malloc, calloc or realloc called with a size of zero (for calloc, a count or an element size of zero) returns a non-null pointer rather than the null pointer, a choice C leaves to the implementation; glibc's malloc and calloc make it, but glibc's realloc of a non-null pointer to size zero frees that pointer and returns the null pointer; relied on here for the call of calloc with count n and element size sizeof(int ), which the analysis cannot show to be both nonzero (24-calloc-zero-count.c:12:7-12:34)
  [Info][Assumption] Used assumption libc.malloc-zero-returns-pointer: malloc, calloc or realloc called with a size of zero (for calloc, a count or an element size of zero) returns a non-null pointer rather than the null pointer, a choice C leaves to the implementation; glibc's malloc and calloc make it, but glibc's realloc of a non-null pointer to size zero frees that pointer and returns the null pointer
