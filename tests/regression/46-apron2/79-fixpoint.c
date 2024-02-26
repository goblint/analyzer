// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval --set ana.base.arrays.domain partitioned

int main ()
{
  char A [3];

  A[2] = 0;

  char *str = A;
  int i = 0;

  while (str[i] != 0) {
    i++;
  }

  return 0;
}
