// PARAM: --enable ana.int.interval --disable solvers.td3.remove-wpoint --set ana.activated[+] unassume --set witness.yaml.unassume 35-hh-ex1b.yml

int main() {
  int i = 0;
  while (i < 100) {
    int j = 0;
    while (j < 100) {
      j++;
      __goblint_check(j <= 100);
    }
    __goblint_check(j == 100);
    i++;
    __goblint_check(i <= 100);
  }
  __goblint_check(i == 100);
  return 0;
}
