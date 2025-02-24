// PARAM: --enable ana.int.bitfield --enable ana.int.enums --enable ana.int.congruence --enable ana.int.interval --set ana.int.refinement fixpoint
// FIXPOINT
int main(void)
{
  unsigned int c = 3;
  unsigned int d = 6;
  unsigned int e = 9;
  int burgo;

  while (1) {
    c ++;

    d += 2U;

    e += 3U;

    assert(e == (unsigned int )c + d);

    burgo = 23;
  }

  return (0);

}