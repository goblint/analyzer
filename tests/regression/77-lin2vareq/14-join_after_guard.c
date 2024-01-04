//SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.relation.privatization top --set sem.int.signed_overflow assume_none
// from 63-affeq/04-join-after-guard
int main(void) {
   int zero = 0;
   int x = 0;

   int t, r, d;
   if (t) {
       r = 10;
       d = 500;
   } else {
       r = 10;
   }
   __goblint_check(r == 10); //SUCCESS
   __goblint_check(d == 500); //UNKNOWN!
   return 0;
}
