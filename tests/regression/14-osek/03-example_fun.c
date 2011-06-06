// PARAM: --oil 02-example.oil --tramp 02-defaultAppWorkstation/tpl_os_generated_configuration.h

// #include "02-defaultAppWorkstation/tpl_os_generated_configuration.h"


int x;
int y;
int z;
int t;

ISR( ii ) {
   GetResource(ri);
    z = 20;
   ReleaseResource(ri);
   return;
}


ISR( i) {
   GetResource(r);
   y++;
   x--;
   ReleaseResource(r);
   return;
}

void g(void){   
   ReleaseResource(r);
   z= 2*t;
   GetResource(r);
   return;
}    

void f(void){   
   GetResource(r);
   t=x+y;
   x=t-x;
   g(); 
   y=t-y;
   ReleaseResource(r);
   return;
}

TASK(t) {
   GetResource(ri);
   x=10;
   y=0;
   ReleaseResource(ri);
   f();
   TerminateTask();
   return;
}

