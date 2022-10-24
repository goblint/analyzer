// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[-] threadflag
// Minimized from sv-benchmarks/c/systemc/pipeline.cil-1.c
#include <assert.h>

int main_clk_pos_edge;
int main_in1_req_up;

int main()
{
  // main_clk_pos_edge = 2; // TODO: uncomment to unskip apron test
  if (main_in1_req_up == 1) // TODO: both branches are dead
    assert(0); // TODO: uncomment to unskip apron test, FAIL (unreachable)
  else
    assert(1); // reachable
  return (0);
}
