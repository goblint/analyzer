// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint --set solver slr4
// Example from Amato-Scozzari, SAS 2013
// Localized narrowing with restart policy should be able to prove that
// 0 <= i <= 10 inside the inner loop.
void main() __attribute__((goblint_precision("no-def_exc","interval")));

void main()
{
   int i = 0;
   while (1) {
      i++;
      for (int j=0; j < 10; j++) {
         assert(0 <= i); // UNKNOWN
         assert(i <= 10);
      }
      if (i>9) i=0;
   }
   return;
}
