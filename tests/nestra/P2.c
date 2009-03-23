main () {
  int y, a, b;
  int *p, *q;
  q = &a;
  p = q;
  *p = 5;
  p = &b;
  y = *q;
}