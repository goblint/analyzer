// PARAM: --oil 01-privatize.oil l --tramp 01-defaultAppWorkstation/tpl_os_generated_configuration.h

// #include "01-defaultAppWorkstation/tpl_os_generated_configuration.h"

int x;
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
   assert(x == 2);
   x--;
   ReleaseResource(r);
   return;
}

TASK(t) {
   y=0;
   x=1;
   y=x;
   assert(y == 1);
   return;
}
