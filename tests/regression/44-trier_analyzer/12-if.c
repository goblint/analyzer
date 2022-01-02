extern int printf();
extern int scanf();

main () {
  int x;
  scanf("%d",&x);
  if (x == 0) {
    assert(x==0);
    printf("Equal to zero.\n");
  }
  else {
    assert(x!=0);
    printf("Non-zero.\n");
  }
  return 0;
}
