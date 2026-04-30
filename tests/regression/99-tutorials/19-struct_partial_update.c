// SKIP TERM PARAM: --enable ana.wp_run

struct Pair {
  int a;
  int b;
};

int main()
{
  struct Pair p;
  p.b = 42; 
  p.a = 1; 
  return p.b;
}
