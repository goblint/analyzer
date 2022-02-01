#if !defined(GOBLINT_NO_QSORT) || !defined(GOBLINT_NO_BSEARCH)
#include <stddef.h>
#endif

#ifndef GOBLINT_NO_QSORT
void qsort(void *ptr, size_t count, size_t size, int (*comp)(const void*, const void*)) __attribute__((goblint_stub));
void qsort(void *ptr, size_t count, size_t size, int (*comp)(const void*, const void*)) {
  // call all possible compares first, before invalidating array elements
  for (size_t i = 0; i < count; i++) {
    for (size_t j = 0; j < count; j++) {
      comp(ptr + i * size, ptr + j * size);
    }
  }

  // randomly swap all possible, invalidates array elements
  for (size_t i = 0; i < count; i++) {
    for (size_t j = 0; j < count; j++) {
      int r; // rand
      if (r) {
        // swap elements byte-by-byte, no other way to do it, because we cannot allocate and copy/swap abstract elements
        for (size_t k = 0; k < size; k++) {
          char *a = ptr + i * size + k;
          char *b = ptr + j * size + k;
          char c = *a;
          *a = *b;
          *b = c;
        }
      }
    }
  }

  // array isn't actually sorted! just pretend calls for Goblint
}
#endif

#ifndef GOBLINT_NO_BSEARCH
void* bsearch(const void *key, const void *ptr, size_t count, size_t size, int (*comp)(const void*, const void*))  __attribute__((goblint_stub));
void* bsearch(const void *key, const void *ptr, size_t count, size_t size, int (*comp)(const void*, const void*)) {
  // linear search for simplicity
  for (size_t i = 0; i < count; i++) {
    const void *a = ptr + i * size;
    if (comp(key, a) == 0) {
      return a;
    }
  }

  return NULL;
}
#endif
