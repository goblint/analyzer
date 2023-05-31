extern int printf();
extern int scanf();

main () {
  int i;
  scanf("%d",&i);
  if (i < 0) {
    if (i % 2 == 0)
      i *= 2;
    else
      i *= 4;
  } else {
    if (i % 2 == 0)
      i *= 8;
    else
      i *= 16;
  }
  printf("%d\n",i);
}
