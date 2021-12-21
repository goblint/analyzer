main() {
  int y;
  int *p, *q;
  *p = 5; // this should generate a warning! segfault!
  y = *q;
}

