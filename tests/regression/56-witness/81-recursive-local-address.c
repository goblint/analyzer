// CRAM

static void f(int *p) {
  int x;
  if (p)
    *p = 1; // p points to the caller's x, not this frame's x.
  else
    f(&x);
}

int main(void) {
  int y = 0;
  int *q = &y;
  f(0);
  return *q;
}
