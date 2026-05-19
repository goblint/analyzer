  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 1 --enable justcil --set dbg.justcil-printer clean 08-bad.c
  int main(void) 
  { 
    int m ;
  
    {
    {
    goto switch_default;
    switch_default: /* CIL Label */ 
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      __loop_condition: /* CIL Label */ 
      if (! 0) {
        goto while_break;
      }
    }
    while_break: /* CIL Label */ ;
    }
    switch_break: /* CIL Label */ ;
    }
    goto lab;
    lab: 
    {
    while (1) {
      while_continue___0: /* CIL Label */ ;
      __loop_condition___0: /* CIL Label */ 
      if (! 0) {
        goto while_break___0;
      }
    }
    while_break___0: /* CIL Label */ ;
    }
    return (0);
  }
  }
