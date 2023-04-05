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

typedef struct {
  int x;
  int y;
} tuple;

void array_with_tuple_offsets(){
  tuple ts[10];

  __goblint_check(&(ts[3].x) == &(ts[3].x));
  __goblint_check(&(ts[3].x) != &(ts[3].y));
  __goblint_check((void*) &(ts[3]) != (void*) &(ts[3].y));
}

int main() {
  struct s a;
  void *p = &a.fst;
  void *q = ((int(*)[1]) (&a))[0];
  // as an __goblint_check, this passes when compiled
  __goblint_check(p == q);

  blob_offsets();
  array_offsets();
  array_with_tuple_offsets();
  return 0;
}
