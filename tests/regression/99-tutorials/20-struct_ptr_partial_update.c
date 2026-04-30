// SKIP TERM PARAM: --enable ana.wp_run

struct Pair {
  int a;
  int b;
};

int main()
{
  struct Pair p;
  struct Pair *pp = &p;

  pp->b = 42;
  pp->a = 1;

  return p.b;
}
