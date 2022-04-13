extern int printf(char *, ...);

typedef struct {
  float re;
  float im;
} complex;

void init (complex *c, float x, float y) {
  c -> re = x;
  c -> im = y;
}

void print (complex *c) {
  printf("%f+%fi.\n", c -> re, c -> im);
}

main () {
  complex c, d;
  c.re = 0;
  init(&c, 1, 4);
  init(&d, 2, 2);
  c = d;
  d.im = -3;
  assert(d.re == 1); // TODO (float)
  print(&c);
}
