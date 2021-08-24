// PARAM: --enable ana.int.interval --enable exp.partition-arrays.enabled --set ana.activated "['base', 'mallocWrapper', 'escape', 'expRelation', 'var_eq']" --sets exp.privatization none
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
