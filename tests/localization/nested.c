// Example from Amato-Scozzari, SAS 2013
// Localized widening should be able to prove that i=10 at the end
// of the nested loops.

void main() 
{
   for (int i=0; i<10 ; i++) 
     for (int j = 0; j < 10 ; j++) ;     
   return;
}

