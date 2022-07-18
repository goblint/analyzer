//PARAM: --enable ana.context.widen
//Needs context widening even with only def_exc
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
