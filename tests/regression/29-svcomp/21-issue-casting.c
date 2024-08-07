// PARAM: --set ana.activated ["'base'","'mallocWrapper'"] --set ana.base.privatization none
// minimal analyses to reveal bug
// none privatization because mutex deactivated
// NOTIMEOUT
static long main(void)
{
  unsigned int cmd;
  int __cil_tmp12 ;

  if ((unsigned int )((int )cmd) == (unsigned int )__cil_tmp12) {
    cmd = 17;
  }

  return 0;
}
