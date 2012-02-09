// PARAM: --oil 05-pi.oil --tramp 05-pi-tramp.h --propset int_domain interval --propdel int_domain trier --solver effectWNcon

// #include "05-pi-tramp.h"

int x = 0;
// int y;
int z = 0;
// int t;

ISR( i2 ) {
   GetResource(r2);
   z = 0;
   assert(z == 0); //
   ReleaseResource(r2);
   return;
}

ISR( i1 ) {
   GetResource(r1);
   if (x >0) {
     x--;     
   }
   assert(x >= 0);
   ReleaseResource(r1);   
   return;
}

ISR( i0 ) {
   GetResource(r1);
   x++;
   if (x >16) {
     x=0;     
   }
   assert(x <= 16);
   ReleaseResource(r1);   
   return;
}

TASK(t) {
   GetResource(r2);
   x = 0;
   z = 0;
   ReleaseResource(r2);
   z=1;
   assert(x <= 16); //
   assert(x >= 0); //
   assert(z <= 1); //
   assert(z >= 0); //
   return;
}
