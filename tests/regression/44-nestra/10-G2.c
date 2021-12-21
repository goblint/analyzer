extern int printf(char *, ...);
extern int scanf(char *, ...);

int i;

main () {
  i = -2;
  scanf("%d",&i);
  printf("The square is  %d .\n", i * i);
}
