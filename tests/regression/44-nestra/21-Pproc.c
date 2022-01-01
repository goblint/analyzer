//PARAM: --enable ana.int.interval
extern int printf();

void add (int *a, int *b) {
  *a += *b;
}

void inc (int *z) {
  int i;
  i = 1;
  add(z, &i);
}

void a (int *x, int *y) {
  add(x, y);
  inc(y);
}

main () {
  int sum, i;
  sum = 0;
  i = 1;
  while (i < 11)
    a(&sum, &i);

  assert(i == 11);
  printf("%d\n", sum);
}
