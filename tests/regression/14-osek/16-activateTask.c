// NOMARSHAL PARAM: --set ana.activated "['base','threadid','threadflag','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set','mallocWrapper']" --set ana.osek.oil 16-activateTask.oil --set ana.osek.taskprefix function_of_ --set ana.osek.isrprefix function_of_

int x = 1;
int t;

TASK(t) {
   x=1;
   assert(x == 1);
   ActivateTask(t2);
   assert(x == 1); // UNKNOWN
   return;
}

TASK(t2) {
   x = 0;
   return;
}
