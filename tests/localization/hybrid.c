// Example from Amato-Scozzari, SAS 2013
// Localized narrowing with restart policy should be able to prove that 
// 0 <= i <= 10 inside the inner loop.

void main() 
{
   int i = 0;
   while (1) {
      i++;
      for (int j=0; j < 10; j++) ;      
      if (i>9) i=0;
   }
   return;
}

