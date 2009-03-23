void rec (int x) {
  char v;
  if (x) {
    v = 'a';
    rec (0);
  } else
    v = 'b';
}

main () {
  rec(1);
}
