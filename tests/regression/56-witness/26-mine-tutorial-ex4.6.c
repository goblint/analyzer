// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 26-mine-tutorial-ex4.6.yml

int main() {
  int x = 40;
  while (x != 0) {
    __goblint_check(x <= 40);
    x--;
    __goblint_check(x >= 0);
  }
  __goblint_check(x == 0); // no witness needed, just by invariant
  return 0;
}
