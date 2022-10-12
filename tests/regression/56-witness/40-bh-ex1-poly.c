// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set ana.activated[+] unassume --set witness.yaml.unassume 40-bh-ex1-poly.yml --set sem.int.signed_overflow assume_none
// TODO: why need to assume no overflow?
int main() {
  int i = 0;
  while (i < 4) {
    int j = 0;
    while (j < 3) {
      i++;
      j += 2;
      __goblint_check(0 <= j);
      __goblint_check(j <= 2 * i);
      __goblint_check(2 * i <= j + 6);
      __goblint_check(j <= 4);
    }
    __goblint_check(0 <= j);
    __goblint_check(j <= 2 * i);
    __goblint_check(2 * i <= j + 6);
    __goblint_check(j <= 4);
    i = i - j / 2 + 1;
  }
  return 0;
}
