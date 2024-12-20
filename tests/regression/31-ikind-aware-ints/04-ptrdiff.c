// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated[+] var_eq
// NOCRASH
int *tmp;

int main ()
{
  int pathbuf[2];

  int *bound = pathbuf + sizeof(pathbuf)/sizeof(*pathbuf) - 1;

  int *p = pathbuf;

  while (p <= bound) {

    *p = 1;

    p++;
  }

  return 0;
}
