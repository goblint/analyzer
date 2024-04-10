// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <goblint.h>

int main(void) {
  int b;
  int a;
  int c; // c is an unknown value
  a = a % 8; // a is in the interval [-7, 7]

  b = c; // no overflow
  __goblint_check(b == c);// SUCCESS

  b = c * 1; // no overflow
  __goblint_check(b == c);// SUCCESS

  b = c ? c : c; // no overflow
  __goblint_check(b == c);// SUCCESS

  b = a + 2; // no overflow
  __goblint_check(b == a + 2);// SUCCESS

  b = c + 2; // might overflow
  __goblint_check(b == c + 2);// UNKNOWN!

  b = a - 2; // no overflow
  __goblint_check(b == a - 2);// SUCCESS

  b = c - 2; // might overflow
  __goblint_check(b == c - 2);// UNKNOWN!

  b = a * 2 - a * 1; // no overflow
  __goblint_check(b == a);// SUCCESS

  b = c * 2 - c * 1; // might overflow
  __goblint_check(b == c); // UNKNOWN!

  b = (-a) + a; // no overflow
  __goblint_check(b == 0); // SUCCESS

   b = (-c) + c; // might overflow
  __goblint_check(b == 0); //  UNKNOWN!

}
