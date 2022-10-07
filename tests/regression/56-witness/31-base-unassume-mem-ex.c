// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 31-base-unassume-mem-ex.yml

int main() {
  int i, j;
  int *p;
  int r; // rand
  i = 0;
  j = 0;
  if (r)
    p = &i;
  else
    p = &j;

  switch (r) {
    case 0:
      __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(j == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(i >= 0);
      __goblint_check(j >= 0);
      __goblint_check(p == &i || p == &j);
      break;
    case 1:
      __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(j == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(i >= 0);
      __goblint_check(j >= 0);
      __goblint_check(p == &i || p == &j);
      break;
    case 2:
      __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(j == 0); // UNKNOWN (intentional by unassume)
      __goblint_check(i >= 0);
      __goblint_check(j >= 0);
      __goblint_check(p == &i || p == &j);
      break;
    default:
      break;
  }
  return 0;
}