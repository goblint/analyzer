// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set ana.activated[+] unassume --set witness.yaml.unassume 29-mine-tutorial-ex4.10.yml
// Using Apron polyhedra to have no narrowing.

int main() {
  int v = 1; // Not explicitly stated in Miné's example
  while (v <= 50) {
    __goblint_check(1 <= v);
    v += 2;
    __goblint_check(v <= 52);
  }
  __goblint_check(51 <= v);
  __goblint_check(v <= 52);
  return 0;
}
