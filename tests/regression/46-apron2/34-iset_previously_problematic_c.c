// SKIP PARAM: --enable ana.int.interval_set  --set ana.base.arrays.domain partitioned --set ana.activated[+] apron
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
// NOCHECK
int main(int argc, char const *argv[])
{
  int i = 0;
  int j;
  int nGroups;
  int bliblablue;
  int inUse16[16] ;


  nGroups = 2;
  bliblablue = 4;

  i = 0;

  while (i < 16) {
    inUse16[i] = 0;

    j = 0;
    while (j < 16) {
      j++;
    }

    i++;
  }

  return 0;
}
