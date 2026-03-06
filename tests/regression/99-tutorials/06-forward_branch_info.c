// SKIP TERM PARAM: --enable ana.wp_run 

int main()
{
  int x = 1;
  int y = 2; // this assignment should yield a warning, as the path where y is used is never taken
  int z = 0; 

  if (z) {
    x = x + y;
  } 

  return x;
}
