extern int printf();

main () {
  int sum = 0;
  int i;
  for (i = 0; i < 100; i++) {
    if (i < 11)
      sum += i;
    else
      break;
  }
  printf("%d\n",sum);
}
