// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a, int b) 
{
  if (a < 0) {
    return a + b;
  } else {
    return a;
  }
}

int g(int a, int b) 
{ 
  if (a > 0) {
    return a - b;
  } else {
    return a;
  }
}
int main()
{
  int x = 0;
  int y = 1;
  int *c = &x;
  int (*h) (int, int) = &f;
  
  if (*c) {
    h = &g;
  }

  *c = 2;
  int z = h(x, y);
  return z;
}
