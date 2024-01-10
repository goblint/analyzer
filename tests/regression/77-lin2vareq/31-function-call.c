// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

int myfunction(int x, int y){
   if (x == 0) {
    __goblint_check(x == 0); // SUCCESS
  } else if (y - x == 3) {
    __goblint_check(y == x + 0); // FAILURE
    __goblint_check(y - x == 3); // SUCCESS
  }

  return 5;
}



int main(void) {
  int x, y, z;
  z = myfunction(x,y);
  return 0;
}
