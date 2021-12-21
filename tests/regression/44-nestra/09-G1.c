extern int printf(char *, ...);

int i;

int proc() {
  int i;
  for (i = 0; i < 1000000000; i++);
  return 0;
}

main () {
  i = proc();
  printf("%d\n", i);
}

