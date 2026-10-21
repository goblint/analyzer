// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none
#include <goblint.h>

int main(void) {
  int n, keep_going;
  int x = n + 5;

  while (keep_going) {
    __goblint_check(x - n == 5);
    keep_going = 0;
  }

  return 0;
}
