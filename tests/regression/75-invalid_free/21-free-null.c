// free and realloc of a null pointer are defined (C11 7.22.3.3: free(NULL)
// does nothing; 7.22.3.5: realloc(NULL, size) behaves as malloc(size)), so
// passing one is not an invalid deallocation.
#include <stdlib.h>

int main(void) {
  free(NULL); //NOWARN

  void *p = NULL;
  free(p); //NOWARN

  int *q = 0;
  free(q); //NOWARN

  void *r = realloc(NULL, sizeof(int)); //NOWARN
  free(r); //NOWARN

  int a;
  int *s = &a;
  free(s); //WARN
  return 0;
}
