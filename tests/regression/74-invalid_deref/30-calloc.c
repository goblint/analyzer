//PARAM: --set ana.activated[+] useAfterFree --set ana.activated[+] threadJoins --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.base.arrays.domain partitioned
#include <pthread.h>
#include <goblint.h>

int main(int argc, char **argv)
{
  int* ptrCalloc = calloc(100UL,8UL);
  *ptrCalloc = 8; //NOWARN
}
