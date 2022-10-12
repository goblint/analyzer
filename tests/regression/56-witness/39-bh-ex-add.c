// PARAM: --enable ana.int.interval --enable ana.sv-comp.functions --set ana.activated[+] unassume --set witness.yaml.unassume 39-bh-ex-add.yml
extern _Bool __VERIFIER_nondet_bool();

int main() {
  int m = 0;
  int n = 0;
  while (1) {
    __goblint_check(m <= 60);
    __goblint_check(n <= 60);
    if (__VERIFIER_nondet_bool()) {
      if (__VERIFIER_nondet_bool()) {
        if (m < 60) {
          m++;
        }
        else {
          __goblint_check(m == 60);
          m = 0;
        }
      }
    }
    if (__VERIFIER_nondet_bool()) {
      if (__VERIFIER_nondet_bool()) {
        if (n < 60) {
          n++;
        }
        else {
          __goblint_check(n == 60);
          n = 0;
        }
      }
    }
  }
  return 0;
}
