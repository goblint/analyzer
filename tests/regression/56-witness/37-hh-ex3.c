// SKIP PARAM: --set ana.activated[+] apron --disable solvers.td3.remove-wpoint --set ana.activated[+] unassume --set witness.yaml.unassume 37-hh-ex3.yml

int main() {
  int i = 0;
  while (i < 4) {
    int j = 0;
    while (j < 4) {
      i++;
      j++;
      __goblint_check(0 <= j);
      __goblint_check(j <= i);
      __goblint_check(i <= j + 3);
      __goblint_check(j <= 4);
    }
    __goblint_check(0 <= j);
    __goblint_check(j <= i);
    __goblint_check(i <= j + 3);
    __goblint_check(j <= 4);
    i = i - j + 1;
  }
  return 0;
}
