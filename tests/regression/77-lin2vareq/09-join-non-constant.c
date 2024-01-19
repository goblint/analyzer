// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

int main(void) {
  int a, b, c, d;

  int t;

  if (t) {
    b = a + 2;
    c = a + 7;
    d = a + 1;
  } else {
    b = a + 30;
    c = a + 35;
    d = a + 10;
  }
  __goblint_check(c == b + 5); // SUCCESS
  __goblint_check(c == b + 3); // FAILURE
  __goblint_check(d == b + 3); // UNKNOWN
  __goblint_check(b == a + 2); // UNKNOWN

  return 0;
}
