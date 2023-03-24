//PARAM: --set ana.malloc.unique_address_count 1
#include <goblint.h>
#include <stdlib.h>

struct s {
  int fst;
};

void blob_offsets(){
  int *a = malloc(10 * sizeof(int));
  int *b = a;

  // Relies on malloc uniqueness, but zero offset can be determined to be equal.
  __goblint_check(a == b);

  // TODO: Determine bitoffsets for blocks (currently top)
  __goblint_check(a + 4 == b + 4); //TODO
  __goblint_check(a == b + 1 ); //TODO
}

void array_offsets(){
  int a[10];
  int *b = a;
  int *c = a + 2;

  __goblint_check(a == b);
  __goblint_check(a + 2 == c);
  __goblint_check(b + 2 == c);

  __goblint_check(a + 1 == b + 1);
  __goblint_check(a + 1 + 2 == c + 1 );
  __goblint_check(b + 1 + 2 == c + 1 );
}


int main() {
  struct s a;
  void *p = &a.fst;
  void *q = ((int(*)[1]) (&a))[0];
  // as an assert, this passes when compiled
  __goblint_check(p == q); //UNKNOWN

  blob_offsets();
  array_offsets();
  return 0;
}
