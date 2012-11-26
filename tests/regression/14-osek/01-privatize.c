// PARAM: --set ana.activated "[['base','escape','OSEK', 'OSEK2', 'OSEK3', 'stack_trace_set']]" --sets ana.osek.oil 01-privatize.oil --sets ana.osek.tramp 01-privatize-tramp.h --sets ana.osek.taskprefix function_of_ --sets ana.osek.isrprefix function_of_

// #include "01-privatize-tramp.h"

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
   assert(x == 2);
   x--;
   ReleaseResource(r);
   assert(x == 1);
   return;
}

TASK(t) {
   y=0;
   assert(y == 0);
   x=1;
   assert(x == 1);
   y=x;
   assert(y == 1);
   return;
}
