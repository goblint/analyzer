// PARAM: --enable ana.int.interval --set solver td3 --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper','apron']" --set exp.privatization none --set exp.apron.privatization dummy --set sem.int.signed_overflow assume_none
// ALSO:  --enable ana.int.interval --set solver slr3 --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper','apron']" --set exp.privatization none --set exp.apron.privatization dummy --set sem.int.signed_overflow assume_none
// Example from Halbwachs-Henry, SAS 2012
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
      assert(i <= j+3);
   }
   return ;
}
