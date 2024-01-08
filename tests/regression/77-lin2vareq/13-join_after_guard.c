//SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.relation.privatization top --set sem.int.signed_overflow assume_none
void main(void) {
   int a, b, c;
   if (a) {
       b = 20;
       c = 250;
   } else {
       b = 20;
   }
   __goblint_check(b == 20);
   __goblint_check(c == 250); //UNKNOWN!
}
