#include <goblint.h>

extern unsigned short const   **__ctype_b_loc(void)  __attribute__((__const__)) ;

int main(void) {
  int i = 5;
  unsigned short const   **tmp ;

  tmp = __ctype_b_loc();
  i = (int)(*((*tmp) + 13));
  __goblint_check(i == 5); // UNKNOWN
  return 0;
}
