extern int printf();
extern int scanf();

main () {
  int x;
  scanf("%d",&x);
  if (x == 0)
    printf("Equal to zero.\n");
  else
    printf("Non-zero.\n");
  return 0;
}
