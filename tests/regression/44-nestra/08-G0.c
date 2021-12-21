extern int printf(char *, ...);
extern int scanf(char *, ...);

int i;

void proc () {
  i = 11;
}

main () {
  proc();
  printf("The square is  %d .\n", i * i);
}
