// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a, int b) {
  
  if (a < 0) {
    return a + b;
  } else {
    return a;
  }

}

int main()
{
  int x = 1;
  int y = 2; // this assignment should yield a warning, as y is not used in the path taken in the called function

  int (*h) (int, int) = &f;

  int z = h(x, y);

  return z;
}
