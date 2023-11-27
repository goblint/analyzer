void main() {
  int y = 0;
  label:
  __goblint_check(y==0);
  goto label;
  y++;
}
