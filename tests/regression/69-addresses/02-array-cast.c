// SKIP
#include <goblint.h>

int main() {
  int a[10];
  int *b = a;

  __goblint_check(a == b);
  __goblint_check(a + 4 == b + 4);

  char *b_char = (char*) a;
  __goblint_check((void*) a == (void*) b_char );

  char* a_intoffset =(char*) (a + 1);
  char* b_intoffset = b_char + sizeof(int);

  __goblint_check(a_intoffset == b_intoffset);
  __goblint_check((char*) (a + 1) == b_char + sizeof(int));

  char* a_4intoffset = (char*) (a + 4);

  __goblint_check(a_4intoffset == b_intoffset); // FAIL
  __goblint_check((char*) (a + 4) == b_char + sizeof(int)); // FAIL

  return 0;
}
