// PARAM: --enable ana.dead-code.branches --set exp.unrolling-factor 2
int main() {
  for (int i = 0; i < 2; i++) { // TODO NOWARN (dead branch)
    if (i == 1) { // TODO NOWARN (dead branch)

    }
  }
  return 0;
}
