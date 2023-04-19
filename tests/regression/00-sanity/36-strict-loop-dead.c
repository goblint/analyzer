// only called with negative n
int basic2(int n) {
  int a = 0;

  if (n < 0)
    return 0;

  for (int i = 0; i < n; i++) // NOWARN
    // Bug in dead code warnings: no dead code warning is emitted, because body is not included
    // in the result. Transformation checks all CFG nodes, and therefore works.
    a += i + n; // NOWARN

  return a; // NOWARN
}


int main() {
  basic2(-3);
}
