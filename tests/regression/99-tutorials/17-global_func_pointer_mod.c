// SKIP TERM PARAM: --enable ana.wp_run 
int (*h) (int);

int g(int a) 
{
  return 0;
}

int f(int a) 
{
  h = &g;
  return a;
}

int main()
{
  int x = 1;
  h = &f;

  int y = h(x);

  return y;
}
