//PARAM: --enable ana.float.interval

//previously failed in line 7 with "exception Invalid_argument("Cilfacade.get_ikind: non-integer type double ")" 
//(same error as in sv-comp: float-newlib/float_req_bl_0220a.c)
int main() {
  double z;

  z = 1 - 1.0;

  assert(z == 0.); // SUCCESS
}
