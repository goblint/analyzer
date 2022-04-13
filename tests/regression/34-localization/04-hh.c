// PARAM: --enable ana.int.interval --set solver slr4
// Example from Amato-Scozzari, SAS 2013
// Localized widening or restart policy should be able to prove that i <= j+3
// if the abstract domain is powerful enough.

void main()
{
   int i = 0;
   while (i<4) {
      int j=0;
      while (j<4) {
         i=i+1;
         j=j+1;
      }
      i = i-j+1;
      assert(i <= j+3); // UNKNOWN
   }
   return ;
}
