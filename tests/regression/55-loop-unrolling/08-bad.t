  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 1 --enable justcil --set dbg.justcil-printer clean 08-bad.c
  [Info] unrolling loop at 08-bad.c:8:7-8:23 with factor 1
  [Info] unrolling loop at 08-bad.c:14:8-14:24 with factor 1
  int main(void) 
  { 
    int m ;
  
    {
    {
    goto switch_default;
    {
    {
    if (! 0) {
      goto loop_end;
    }
    }
    loop_continue_0: /* CIL Label */ ;
    switch_default: /* CIL Label */ 
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! 0) {
        goto while_break;
      }
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    switch_break: /* CIL Label */ ;
    }
    goto lab;
    {
    {
    if (! 0) {
      goto loop_end___0;
    }
    }
    loop_continue_0___0: /* CIL Label */ ;
    lab: 
    {
    while (1) {
      while_continue___0: /* CIL Label */ ;
      if (! 0) {
        goto while_break___0;
      }
    }
    while_break___0: /* CIL Label */ ;
    }
    loop_end___0: /* CIL Label */ ;
    }
    return (0);
  }
  }
