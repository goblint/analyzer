// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a, int b) {
  
  if (a > 0) {
    return a + b;
  } else {
    return a;
  }

}

int main()
{
  int x = 1;
  int y = 2;

  int z = f(x, y);

  return z;
}
