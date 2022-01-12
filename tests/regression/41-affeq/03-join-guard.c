// SKIP PARAM: --set ana.activated[+] affeq --set exp.apron.privatization dummy
void main(void) {
   int zero = 0;
   int x = 0;
   int p = 0;

   if(zero) {
       x = 10;
   } else {
       x = 100;
       p = 5000;
   }

   assert (x == 100);
   assert (p == 5000);

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
