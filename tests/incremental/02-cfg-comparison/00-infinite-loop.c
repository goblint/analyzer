void main()
{ int x;
  int y = 0;

  __goblint_check(y==0);

  while (1) {
    if (x) {
      y++;
    } else {
      y--;
    }
  }
}
