// PARAM: --enable ana.float.interval

// previously failed in line 7 with "exception Invalid_argument("Cilfacade.get_ikind: non-integer type double ")"
//(same error as in sv-comp: float-newlib/float_req_bl_0220a.c)
// similar error also occured in the additional examples when branching on a float argument
int main()
{
  double z;

  z = 1 - 1.0;

  assert(z == 0.); // SUCCESS

  if (0.)
    ;

  if (0 == (0. + 1.))
    ;
  assert(0); // FAIL
}
