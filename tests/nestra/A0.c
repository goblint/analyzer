extern int printf(char *, ...);

main () {
  int i, j;
  int a[11];
  for (i = 0; i < 11; i++)
    a[i] = i;
  i = 0;
  do {
    j = i + 1;
    a[j] += a[i];
    i = j;
  } while (i < 11);
  printf("%d\n", a[10]);
}

