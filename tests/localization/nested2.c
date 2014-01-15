// Example from Amato-Scozzari, SAS 2013
// Localized narrowing should be able to prove that i >= 0 in the outer
// loop.

void main() 
{
   int i = 0;
   while (1) {      
      for (int j=0; j<10; j++) ;
      i=i+11-j;
   }
   return;
}

