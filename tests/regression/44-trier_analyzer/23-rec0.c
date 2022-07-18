extern int printf(char *, ...);
extern int scanf(char *, ...);
extern void exit(int);

int factRec(int p, int n) {
  if (n == 0)
    return p;
  else
    return factRec(n * p, n - 1);
}

int fact (int n) {
  if (n < 0) {
    printf("Error!\n");
    exit(1);
  } else
    return factRec(1,n);
}

main () {
  int n;
  scanf("%d",&n);
  printf("%d\n",fact(n));
}
