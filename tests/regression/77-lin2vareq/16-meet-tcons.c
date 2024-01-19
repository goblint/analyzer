// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none --set ana.int.def_exc false --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false


int main(void) {
  int x, y, z;

  if (x == 0) {
    __goblint_check(x == 0); // SUCCESS
  } else if (y - x == 3) {
    __goblint_check(y == x + 0); // FAILURE
    __goblint_check(y - x == 3); // SUCCESS
  }

  return 0;
}
