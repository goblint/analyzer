// SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.relation.privatization top --set sem.int.signed_overflow assume_none --set ana.int.def_exc false --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false

void main(void) {
  int x1, x2, x3, x4, x5, x6, x7, x8, x9;
  int t;
  if (t) {
    x2 = 2;
    x1 = 3;
    x3 = 4;
    x4 = 5;
    x5 = x6 + 6;
    x7 = x6 + 3;
    x8 = x6 - 55;
  } else {
    x1 = 3;
    x2 = 3;
    x3 = 4;
    x4 = 5;
    x5 = x6 + 11;
    x7 = x6 + 8;
    x8 = x6 - 50;
  }
  __goblint_check(x1 == 3);
  __goblint_check(x2 == 2); // UNKNOWN!
  __goblint_check(x3 == 4);
  __goblint_check(x4 == 5);
  __goblint_check(x7 == x5 - 3);
  __goblint_check(x8 == x7 - 58);
}
