//PARAM: --enable ana.int.interval
extern int printf(char *, ...);

main() {
  int sum = 0;
  int i = 0;
  while (i < 11) {
    sum += i;
    i++;
  }
  assert(i == 11);
  printf("%d\n",sum);
}
