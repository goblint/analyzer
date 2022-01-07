// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint --set solver slr3
// Example from Amato-Scozzari, SAS 2013
// Localized widening should be able to prove that i=10 at the end
// of the nested loops.

void main() __attribute__((goblint_precision("no-def_exc","interval")));

void main()
{
   int i = 0;

   for (; i<10 ; i++) {
     for (int j = 0; j < 10 ; j++) ;
   }

   assert(i == 10);
}
