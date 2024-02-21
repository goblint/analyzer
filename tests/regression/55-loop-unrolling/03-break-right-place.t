  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --enable justcil --set dbg.justcil-printer clean 03-break-right-place.c
  [Info] unrolling loop at 03-break-right-place.c:8:5-15:5 with factor 5
  extern void __goblint_check(int exp ) ;
  extern void __goblint_assume(int exp ) ;
  extern void __goblint_assert(int exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  int main(void) 
  { 
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int j  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    j = 0;
    {
    {
    if (! (i < 17)) {
      goto loop_end;
    }
    if (j == 0) {
      j = 1;
      goto loop_end;
    }
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 17)) {
      goto loop_end;
    }
    if (j == 0) {
      j = 1;
      goto loop_end;
    }
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 17)) {
      goto loop_end;
    }
    if (j == 0) {
      j = 1;
      goto loop_end;
    }
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 17)) {
      goto loop_end;
    }
    if (j == 0) {
      j = 1;
      goto loop_end;
    }
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 17)) {
      goto loop_end;
    }
    if (j == 0) {
      j = 1;
      goto loop_end;
    }
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 17)) {
        goto while_break;
      }
      if (j == 0) {
        j = 1;
        goto while_break;
      }
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(i == 0);
    return (0);
  }
  }
