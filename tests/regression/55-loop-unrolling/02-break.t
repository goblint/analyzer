  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --enable justcil --set dbg.justcil-printer clean 02-break.c
  [Info] unrolling loop at 02-break.c:6:5-15:2 with factor 5
  extern void __goblint_check(_Bool exp ) ;
  extern void __goblint_assume(_Bool exp ) ;
  extern void __goblint_assert(_Bool exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  int main(void) 
  { 
    int r ;
    int i ;
  
    {
    r = 5;
    i = 0;
    {
    __loop_condition___0: /* CIL Label */ 
    if (! (i < 2)) {
      goto loop_end;
    }
    {
    if (i == 0) {
      goto case_0___4;
    }
    if (i == 1) {
      goto case_1___4;
    }
    goto switch_break;
    case_0: /* CIL Label */ 
    goto loop_end;
    case_1: /* CIL Label */ 
    r = 8;
    switch_break: /* CIL Label */ ;
    }
    r = 17;
    goto loop_end;
    i ++;
    loop_continue_0: /* CIL Label */ ;
    __loop_condition___1: /* CIL Label */ 
    if (! (i < 2)) {
      goto loop_end;
    }
    {
    if (i == 0) {
      goto case_0___4;
    }
    if (i == 1) {
      goto case_1___4;
    }
    goto switch_break___0;
    case_0___0: /* CIL Label */ 
    goto loop_end;
    case_1___0: /* CIL Label */ 
    r = 8;
    switch_break___0: /* CIL Label */ ;
    }
    r = 17;
    goto loop_end;
    i ++;
    loop_continue_1: /* CIL Label */ ;
    __loop_condition___2: /* CIL Label */ 
    if (! (i < 2)) {
      goto loop_end;
    }
    {
    if (i == 0) {
      goto case_0___4;
    }
    if (i == 1) {
      goto case_1___4;
    }
    goto switch_break___1;
    case_0___1: /* CIL Label */ 
    goto loop_end;
    case_1___1: /* CIL Label */ 
    r = 8;
    switch_break___1: /* CIL Label */ ;
    }
    r = 17;
    goto loop_end;
    i ++;
    loop_continue_2: /* CIL Label */ ;
    __loop_condition___3: /* CIL Label */ 
    if (! (i < 2)) {
      goto loop_end;
    }
    {
    if (i == 0) {
      goto case_0___4;
    }
    if (i == 1) {
      goto case_1___4;
    }
    goto switch_break___2;
    case_0___2: /* CIL Label */ 
    goto loop_end;
    case_1___2: /* CIL Label */ 
    r = 8;
    switch_break___2: /* CIL Label */ ;
    }
    r = 17;
    goto loop_end;
    i ++;
    loop_continue_3: /* CIL Label */ ;
    __loop_condition___4: /* CIL Label */ 
    if (! (i < 2)) {
      goto loop_end;
    }
    {
    if (i == 0) {
      goto case_0___4;
    }
    if (i == 1) {
      goto case_1___4;
    }
    goto switch_break___3;
    case_0___3: /* CIL Label */ 
    goto loop_end;
    case_1___3: /* CIL Label */ 
    r = 8;
    switch_break___3: /* CIL Label */ ;
    }
    r = 17;
    goto loop_end;
    i ++;
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      __loop_condition: /* CIL Label */ 
      if (! (i < 2)) {
        goto while_break;
      }
      {
      if (i == 0) {
        goto case_0___4;
      }
      if (i == 1) {
        goto case_1___4;
      }
      goto switch_break___4;
      case_0___4: /* CIL Label */ 
      goto switch_break___4;
      case_1___4: /* CIL Label */ 
      r = 8;
      switch_break___4: /* CIL Label */ ;
      }
      r = 17;
      goto while_break;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check((_Bool )(r == 17));
    return (0);
  }
  }
