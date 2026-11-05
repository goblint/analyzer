  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 2 --enable justcil --set dbg.justcil-printer clean 09-weird.c
  [Info] unrolling loop at 09-weird.c:8:5-11:5 with factor 2
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
    int j ;
    int i ;
  
    {
    j = 0;
    i = 0;
    {
    __loop_condition___0: /* CIL Label */ 
    if (! (i < 50)) {
      goto loop_end;
    }
    goto somelab___0;
    somelab___0: 
    j = 8;
    i ++;
    loop_continue_0: /* CIL Label */ ;
    __loop_condition___1: /* CIL Label */ 
    if (! (i < 50)) {
      goto loop_end;
    }
    goto somelab___1;
    somelab___1: 
    j = 8;
    i ++;
    loop_continue_1: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      __loop_condition: /* CIL Label */ 
      if (! (i < 50)) {
        goto while_break;
      }
      goto somelab;
      somelab: 
      j = 8;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check((_Bool )(j == 8));
    return;
  }
  }
