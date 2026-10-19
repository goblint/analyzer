With sem.malloc.zero pointer, each allocation whose size the analysis cannot
show nonzero reports libc.malloc-zero-returns-pointer; malloc(sizeof(int))
reports nothing.

  $ goblint --set sem.malloc.zero pointer 23-malloc-zero-pointer.c 2>&1 | grep Assumption
  [Info][Assumption] libc.malloc-zero-returns-pointer: malloc, calloc or realloc called with a size of zero (for calloc, a count or an element size of zero) returns a non-null pointer rather than the null pointer, a choice C leaves to the implementation; glibc's malloc and calloc make it, but glibc's realloc of a non-null pointer to size zero frees that pointer and returns the null pointer; relied on here for the call of malloc with size n * sizeof(int ), which the analysis cannot show to be nonzero (23-malloc-zero-pointer.c:12:7-12:35)
  [Info][Assumption] libc.malloc-zero-returns-pointer: malloc, calloc or realloc called with a size of zero (for calloc, a count or an element size of zero) returns a non-null pointer rather than the null pointer, a choice C leaves to the implementation; glibc's malloc and calloc make it, but glibc's realloc of a non-null pointer to size zero frees that pointer and returns the null pointer; relied on here for the call of realloc with size n, which the analysis cannot show to be nonzero (23-malloc-zero-pointer.c:16:7-16:25)
  [Info][Assumption] Used assumption libc.malloc-zero-returns-pointer: malloc, calloc or realloc called with a size of zero (for calloc, a count or an element size of zero) returns a non-null pointer rather than the null pointer, a choice C leaves to the implementation; glibc's malloc and calloc make it, but glibc's realloc of a non-null pointer to size zero frees that pointer and returns the null pointer

With the default sem.malloc.zero either, both results are considered and nothing is assumed.

  $ goblint 23-malloc-zero-pointer.c 2>&1 | grep Assumption
  [1]
