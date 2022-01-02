extern int scanf (char *, ...);

main () {
  int a;
  a = 0;
  a = scanf("%d",&a);
  assert(a == 0); //UNKNOWN!
}
