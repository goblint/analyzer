//PARAM: --set ana.activated[+] apron --set ana.activated[-] threadflag --set ana.activated[-] thread --set ana.activated[-] threadid
// Minimized from sv-benchmarks/c/systemc/pipeline.cil-1.c
#include <assert.h>
#include <goblint.h>

int main_clk_pos_edge;
int main_in1_req_up;

int main()
{
  int litmus;
  main_clk_pos_edge = 2;
  if (main_in1_req_up == 1)
    litmus = 0; // unreachable
  else
    litmus = 1;

  __goblint_check(litmus == 1);
  return (0);
}
