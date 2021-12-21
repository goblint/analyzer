extern int printf (char *, ...);

void *awful(int **u) {
  int a;
  a = 5;
  (*u) = &a;
}

main () {
  int *p;
  awful(&p);
  printf("%d\n", *p);
}
