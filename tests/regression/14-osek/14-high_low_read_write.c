//PARAM: --set ana.activated "['base','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set']" --sets ana.osek.oil 07-flags.oil --sets ana.osek.flags[+] flag

int flag = 0;
int var = 0;

int highread = 0;
int highwrite = 0;
int lowread = 0;
int lowwrite = 0;

int highread_bad = 0;
int highwrite_bad = 0;
int lowread_bad = 0;
int lowwrite_bad = 0;


ISR(high) 
{
  GetResource(r);
  ReleaseResource(r);
  int tmp = highread;
  highwrite = 1;
  int tmp = highread_bad;
  highwrite_bad = 1;

  lowread++;
  lowwrite++;
  lowread_bad++;
  lowwrite_bad++;
  
}


ISR(four)
{
  if (flag == 0) {
    var ++;
  }

  
  highread_bad++;
  highwrite_bad++;

  lowread_bad++;
  lowwrite_bad++;
  
}

ISR(two)
{
  flag = 1;
  int temp = var;       // NORACE
  var = temp - var;     // NORACE
//   assert(var = 0);      //
  flag = 0;
  
  highread++;
  highwrite++;
  highread_bad++;
  highwrite_bad++;

  int tmp = lowread;
  lowwrite = 1;
  int tmp = lowread_bad;
  lowwrite_bad = 1;
  
}

TASK(init)
{
  GetResource(r);
//   flag = 0;
  var = 0;
  flag = 0;
  ReleaseResource(r);
}
