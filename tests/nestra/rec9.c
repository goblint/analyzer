void p (int *x) {
  int y;
  int *z;
  z = &y;
  p(z);
}

main() {
  int z;
  p(&z);
}
