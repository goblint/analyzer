// PARAM: --enable ana.sv-comp.functions --enable ana.int.interval

void __VERIFIER_assert(int cond) {
  int x = 1 << 80;
}

int main() {
  int r; // rand
  __VERIFIER_assert(1); // NOWARN
  __VERIFIER_assert(r); // NOWARN
  __VERIFIER_assert(0); // NOWARN
  return 0;
}
