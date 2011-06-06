// PARAM: --oil 01-privatize.oil l --tramp 01-defaultAppWorkstation/tpl_os_generated_configuration.h

// #include "01-defaultAppWorkstation/tpl_os_generated_configuration.h"

int x = 1;
int y;
int z;
int t;

ISR( ii ) {
   x = 1;
   assert(x == 1);
   return;
}


ISR( i) {
   GetResource(r);
   x++;
   x = 2;
   assert(x == 2);
   x--;
   ReleaseResource(r);
   assert(x == 1);
   return;
}

TASK(t) {
   y=0;
   x=1;
   assert(x == 1);
   y=x;
   return;
}
