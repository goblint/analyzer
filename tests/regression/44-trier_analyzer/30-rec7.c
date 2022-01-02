extern int scanf(char *, ...);

/* pointer forward along the stack to a formal parameter */

void rec (int **u, int i) {
  if (i >= 2)
    return;
  else {
    **u = 12;
    rec(u, i + 1);
    // u may be changed in the recursive call!
    assert(**u == 12); //UNKNOWN!
    if (1) {
      int a;
      (*u) = &a;
    }
    return;
  }
}

main () {
  int a;
  int *p;
  p = &a;
  scanf("%d", p);
  rec(&p, a);
}
