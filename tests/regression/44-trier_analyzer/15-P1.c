main () {
  int y, a;
  int *p, *q, *t;
  q = &a;
  p = q;
  t = p;
  *p = 5;
  *t = 7;
  y = *q;
}