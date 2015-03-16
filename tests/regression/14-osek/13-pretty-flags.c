//PARAM: --set ana.activated "['base','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set']" --sets ana.osek.oil 07-flags.oil --sets ana.osek.flags[+] flag

int flag = 0;
int var = 0;
int x = 0;
int y = 0;

ISR(high) 
{
  GetResource(r);
  ReleaseResource(r);
}

ISR(four)
{
  if (flag == 0) {
    var ++;
  } else y=5;
  x ++;
}

ISR(two)
{
  flag = 1;
  int temp = var;       // NORACE
  var = temp - var;     // NORACE
  x--;
//   assert(var = 0);      //
  flag = 0;
}

TASK(init)
{
  GetResource(r);
//   flag = 0;
  var = 0;
  flag = 0;
  ReleaseResource(r);
}
