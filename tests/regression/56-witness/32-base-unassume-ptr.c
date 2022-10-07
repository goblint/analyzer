// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 32-base-unassume-ptr.yml
// TODO: disjunction in invariant
int main() {
  int i, j;
  int *p;
  p = &i;

  __goblint_check(p == &i); // UNKNOWN (intentional by unassume)
  __goblint_check(p == &i || p == &j);
  return 0;
}
