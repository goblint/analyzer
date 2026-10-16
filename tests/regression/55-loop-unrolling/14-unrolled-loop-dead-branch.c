// PARAM: --enable ana.dead-code.branches --set exp.unrolling-factor 2
int main() {
  for (int i = 0; i < 2; i++) { // NOWARN (dead branch)
    if (i == 1) { // NOWARN (dead branch)

    }
  }
  return 0;
}
