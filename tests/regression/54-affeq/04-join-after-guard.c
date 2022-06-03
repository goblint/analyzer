//SKIP PARAM: --set ana.activated[+] affeq --set ana.relation.privatization dummy --set sem.int.signed_overflow assume_none
void main(void) {
   int zero = 0;
   int x = 0;

   int t, r, d;
   if (t) {
       r = 10;
       d = 500;
   } else {
       r = 10;
   }
   assert (r == 10);
   assert (d == 500); //UNKNOWN!
}
