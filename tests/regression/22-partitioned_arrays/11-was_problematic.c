// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

int main(int argc, char **argv)
{
  int unLo;
  int sp = 1;
  int nextD[3];

  // nextD[0] = 2; // When we have this and the one in line 25 it is fine

  int x;

  while (sp > 0)
  {
    sp--;

    while (1)
    {
      if (x+1 <= 100 || x+1 > 100)
      {
        break;
      }
    }

    // If we have only this one there is a problem
    nextD[0] = 2;

    int y = 27;
 }

  __goblint_check(1 == 1); // Was reported as unreachable before
  return 0;
}
