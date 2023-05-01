  $ args='remove_dead_code -- --enable ana.int.interval --enable sem.noreturn.dead_code'

  $ ./transform.sh $args 02-deadcode.c
  extern void abort()  __attribute__((__noreturn__)) ;
  int basic1(int n ) 
  { 
    int a ;
    int b ;
    int c ;
    int i ;
  
    {
    {
    a = 0;
    b = 1;
    }
    {
    i = 0;
    }
    {
    while (1) {
  
      if (! (i < n)) {
        goto while_break;
      }
      {
      c = a + b;
      a = b;
      b = c;
      i ++;
      }
    }
    while_break: /* CIL Label */ ;
    }
    return (a);
  }
  }
  int basic2(int n ) 
  { 
    int a ;
  
    {
    {
    a = 0;
    }
    if (n < 0) {
      return (0);
    }
  }
  }
  int one_branch_dead(int x ) 
  { 
  
  
    {
    if (x > 7) {
      return (x);
    }
  }
  }
  int uncalled_but_referenced_function(int x ) 
  { 
  
  
    {
  
  }
  }
  int conditional_call_in_loop(int x ) 
  { 
    int i ;
  
    {
    {
    i = 0;
    }
    {
    while (1) {
  
      if (! (i < x)) {
        goto while_break;
      }
      {
      i ++;
      }
    }
    while_break: /* CIL Label */ ;
    }
    return (0);
  }
  }
  int loop_continue_break(int u ) 
  { 
    int w ;
    int t ;
  
    {
    {
    w = 12;
    t = 0;
    }
    {
    while (1) {
  
      if (! (t < u)) {
        goto while_break;
      }
      {
      w += t;
      }
      if (t > 3) {
        goto __Cont;
      }
      {
      w += u;
      }
      if (t > 7) {
        goto while_break;
      }
      {
      w += w;
      }
      __Cont: /* CIL Label */ 
      {
      t ++;
      }
    }
    while_break: /* CIL Label */ ;
    }
    return (w);
  }
  }
  int loop_dead_on_break(int z ) 
  { 
    int s ;
    int i ;
  
    {
    {
    s = 5;
    i = 0;
    }
    {
    while (1) {
  
      if (! (i < z)) {
        goto while_break;
      }
      {
      s += i;
      }
      if (i < 5) {
        goto while_break;
      }
    }
    while_break: /* CIL Label */ ;
    }
    return (s);
  }
  }
  int compound_statement_in_out(void) 
  { 
  
  
    {
    goto in1;
    {
    in1: 
    goto in2;
    }
    {
    {
    in2: 
    goto in3;
    }
    }
    {
    {
    in3: 
    goto out;
    }
    }
    out: 
    return (0);
  }
  }
  int main(void) 
  { 
    int (*f)(int  ) ;
  
    {
    {
    basic1(7);
    basic1(3);
    basic1(6);
    basic2(-3);
    basic2(-6);
    basic2(-12);
    one_branch_dead(9);
    one_branch_dead(12);
    f = & uncalled_but_referenced_function;
    conditional_call_in_loop(5);
    loop_continue_break(11);
    loop_dead_on_break(3);
    compound_statement_in_out();
    abort();
    }
  }
  }

Transformation still works with 'exp.mincfg', but can not find all dead code; test against the diff.
Macintosh's diff(1) adds whitespace after the function names, strip with sed.
  $ diff p -U0 "$(./transform.sh --file $args 02-deadcode.c)" "$(./transform.sh --file $args --enable exp.mincfg 02-deadcode.c)" | sed 's/[[:blank:]]*$//' | tail +3
  @@ -13,0 +14,3 @@ int basic1(int n )
  +  if (n < 0) {
  +    return (0);
  +  }
  @@ -54,0 +58,2 @@ int one_branch_dead(int x )
  +  } else {
  +    return (7 - x);
  @@ -65,0 +71,8 @@ int uncalled_but_referenced_function(int
  +int uncalled1(void) 
  +{ 
  +
  +
  +  {
  +
  +}
  +}
  @@ -79,0 +93,5 @@ int conditional_call_in_loop(int x )
  +    if (i > 7) {
  +      {
  +      uncalled1();
  +      }
  +    }
  @@ -151,0 +170,4 @@ int loop_dead_on_break(int z )
  +    {
  +    s += s;
  +    i ++;
  +    }
  @@ -203,0 +226,2 @@ int main(void)
  +  uncalled1();
  +  uncalled_but_referenced_function(3);
