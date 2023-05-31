extern int printf (char *, ...);
extern int scanf (char *, ...);

main () {
  int c[10];
  int *a, *b;
  int x, i;
  b = c;
  for (i = 0; i < 10; i++)
    scanf("%d",&(b[i]));
  a = b;
  x = a[0];
  printf("%d\n",x);
}
