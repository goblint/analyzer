  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --enable justcil 05-continue.c
  [Info] unrolling loop at 05-continue.c:9:5-17:5 with factor 5
  extern void __goblint_check(int exp ) ;
  extern void __goblint_assume(int exp ) ;
  extern void __goblint_assert(int exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  void main(void) 
  { 
    int j  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    j = 0;
    i = 0;
    {
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    if (i < 2) {
      goto __Cont___0;
    }
    if (i > 4) {
      goto loop_end;
    }
    j ++;
    __Cont___0: /* CIL Label */ 
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    if (i < 2) {
      goto __Cont___1;
    }
    if (i > 4) {
      goto loop_end;
    }
    j ++;
    __Cont___1: /* CIL Label */ 
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    if (i < 2) {
      goto __Cont___2;
    }
    if (i > 4) {
      goto loop_end;
    }
    j ++;
    __Cont___2: /* CIL Label */ 
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    if (i < 2) {
      goto __Cont___3;
    }
    if (i > 4) {
      goto loop_end;
    }
    j ++;
    __Cont___3: /* CIL Label */ 
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    if (i < 2) {
      goto __Cont___4;
    }
    if (i > 4) {
      goto loop_end;
    }
    j ++;
    __Cont___4: /* CIL Label */ 
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 50)) {
        goto while_break;
      }
      if (i < 2) {
        goto __Cont;
      }
      if (i > 4) {
        goto while_break;
      }
      j ++;
      __Cont: /* CIL Label */ 
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(j == 3);
    return;
  }
  }
