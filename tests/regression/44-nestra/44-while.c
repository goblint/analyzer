extern int printf(char *, ...);

main() {
  int sum = 0;
  int i = 0;
  while (i < 11) {
    sum += i;
    i++;
  }
  printf("%d\n",sum);
}
