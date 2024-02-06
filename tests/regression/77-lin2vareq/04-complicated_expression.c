// SKIP PARAM: --set sem.int.signed_overflow assume_none --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false --set ana.activated[+] lin2vareq

#include <stdio.h>
#include <goblint.h>


int main() {
  int x;
  int k;
  if (x < 300 && k < 300) {
    int y = 5;

    int result1 = 3 * (x + y) - 2 * x + 6;
    int result2 = 3 * (x + y) - 2 * k + 6;
    int result3 = x * 3 - x * 2;
    int result4 = x * 3 - x * k * x;

    __goblint_check(result1 == x + 21);            // SUCCESS
    __goblint_check(result2 == x + 21);            // UNKNOWN!
    __goblint_check(result3 == x);                 // SUCCES
    __goblint_check(result4 == x * 3 - x * k * x); // UNKNOWN
  }
  return 0;
}
