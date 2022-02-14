// SKIP
// TODO: fix pseudo return handling in CFG comparison
void main()
{ int x;
  int y = 0;

  assert(y==0);

  while (1) {
    if (x) {
      y++;
    } else {
      y--;
    }
  }
}
