// SKIP PARAM: --enable ana.int.interval_set --set ana.base.arrays.domain partitioned --set ana.activated[+] apron
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
// NOCHECK
char buf2[67];

int main(int argc, char **argv)
{
  int human_output_opts;
  int to_block_size;
  char buf1[67];
  char local;

  int from_block_size = 1;

  int exponent;
  int exponent_max;
  int buflen;
  int power;

  memmove((void *)buf2, (void const *)buf1, 67);

  int bla = 18;

  if (human_output_opts & 128)
  {
    if (exponent < 0)
    {
      exponent = 0;
      power = 1;
      while (power < to_block_size)
      {
        exponent++;
        if (exponent == exponent_max)
        {
          break;
        }
      }
    }
  }
  return 0;
}
