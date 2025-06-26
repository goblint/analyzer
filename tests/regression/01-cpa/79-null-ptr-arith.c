int main(void) {
  int *p;
  int i;
  int *q = p + i;
  *q = 42; // WARN (may NULL dereference)

  if (p) {
    q = p + i;
    *q = 42; // WARN (may NULL dereference)

    if (i == 0) {
      q = p + i;
      *q = 42; // NOWARN (no NULL dereference)
    }
    else {
      q = p + i;
      *q = 42; // WARN (may NULL dereference)
    }
  }
}
