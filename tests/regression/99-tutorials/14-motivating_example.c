// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a, int b) 
{
  return a + b;
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
  int x = 1;
  int y = 2;
  int *c = &x;
  int (*h) (int, int) = &f;
  
  if (*c) {
    h = &g;
  }

  *c = 0;
  int z = h(x, y);
  return z;
}
