// SKIP
#include <goblint.h>

int main() {
  int a[10];
  int *b = a;

  assert(a == b);
  assert(a + 4 == b + 4);

  char *b_char = (char*) a;
  assert((void*) a == (void*) b_char );

  char* a_intoffset = a + 1;
  char* b_intoffset =  b_char + sizeof(int);

  __goblint_check(a_intoffset == b_intoffset);
  __goblint_check((char*) (a + 1) == b_char + sizeof(int));
  return 0;
}
