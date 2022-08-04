extern int printf (char *, ...);
extern int scanf (char *, ...);

typedef struct {
  float re;
  float im;
} complex;

main () {
  complex c, d;
  float x;
  scanf("%d",&(c.re));
  scanf("%d",&(c.im));
  c.re = 1;
  d = c;
  x = d.im;
  __goblint_check(d.re == 1); //TODO (float)
  printf("%d\n",x);
}
