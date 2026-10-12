// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// Upjumping goto, even though #line makes the goto appear below its label
int main()
{
  int num = 1;

#line 1000
loop:
  num++;

#line 900
  goto loop;

  return 0;
}
