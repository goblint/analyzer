extern int scanf(char *, ...);

int *rec(int i) {
  int *p;
  if (!i)
    p = rec(i + 1);
  return &i;
}

main () {
  int *p;
  int i;
  scanf("%d", &i);
  p = rec(i);
}
