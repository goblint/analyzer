extern int printf(char *, ...);
extern int scanf(char *, ...);

int i;

void proc () {
  i = 11;
}

main () {
  proc();
  assert(i == 11);
  printf("The square is  %d .\n", i * i);
}
