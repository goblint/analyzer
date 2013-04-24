// PARAM: --set ana.activated "[['base','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set']]" --sets ana.osek.oil 02-example.oil --sets ana.osek.taskprefix function_of_ --sets ana.osek.isrprefix function_of_

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

TASK(t) {
   GetResource(ri);
   x=10;
   y=0;
   ReleaseResource(ri);
   GetResource(r);
   t=x+y;
   x=t-x;
   ReleaseResource(r);
   z= 2*t; //RACE
   GetResource(r);
   y=t-y;
   ReleaseResource(r);
   TerminateTask();
return;}
