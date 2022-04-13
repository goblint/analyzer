extern int scanf(char *, ...);

/* pointer backward along the stack to a local variable */

void rec (int *p) {
  int i;
  if (*p)
    return;
  else {
    (*p)++;
    i = (*p);
    p = &i;
    rec(p);
    assert(p == &i); //UNKNOWN!
    return;
  }
}

main () {
  int a;
  scanf("%d",&a);
  rec(&a);
}
