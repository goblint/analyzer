//PARAM: --enable ana.context.widen
extern int scanf(char *, ...);
extern int printf(char *, ...);

void rec(int i) {
  int a;
  scanf("%d", &a);
  if (a > 0)
     rec(i + 1);
  else {
     printf("You gave  %d  positive numbers!\n", i);
     return;
  }
}

main () {
  rec(0);
}
