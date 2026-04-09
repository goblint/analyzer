// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a) 
{
  int b = 3; // no warning, as b is used in one call of f

  if (a < 5) {
    return a + b;
  } else {
    return a;
  }
}

int main()
{
  int x = 0;

  int u = f (x);

  x = 10;

  int v = f (x); 
  return u + v;
}
