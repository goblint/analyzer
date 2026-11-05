  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5 --enable justcil --set dbg.justcil-printer clean 04-simple.c
  [Info] unrolling loop at 04-simple.c:10:5-13:5 with factor 5
  extern void __goblint_check(_Bool exp ) ;
  extern void __goblint_assume(_Bool exp ) ;
  extern void __goblint_assert(_Bool exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  void main(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i ;
  
    {
    i = 0;
    {
    __loop_condition___0: /* CIL Label */ 
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    loop_continue_0: /* CIL Label */ ;
    __loop_condition___1: /* CIL Label */ 
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    loop_continue_1: /* CIL Label */ ;
    __loop_condition___2: /* CIL Label */ 
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    loop_continue_2: /* CIL Label */ ;
    __loop_condition___3: /* CIL Label */ 
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    loop_continue_3: /* CIL Label */ ;
    __loop_condition___4: /* CIL Label */ 
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      __loop_condition: /* CIL Label */ 
      if (! (i < 5)) {
        goto while_break;
      }
      a[i] = i;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check((_Bool )(a[0] == 0));
    __goblint_check((_Bool )(a[3] == 3));
    return;
  }
  }
