//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
//  special input triggering add_dimarray of [0,0] at line 10; visible via --trace modify_dims
//      both occurances of variables need to be bumped/shifted by 2 indices
//  in our case, d = c still needs to hold
int a;
b() {}
void main() {
  int c;
  int d = c;
  b();
  __goblint_check(d == c); //SUCCESS
}
