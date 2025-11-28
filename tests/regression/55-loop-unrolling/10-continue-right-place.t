  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 11 --enable justcil --set dbg.justcil-printer clean 10-continue-right-place.c
  [Info] unrolling loop at 10-continue-right-place.c:7:3-15:3 with factor 11
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
    int i ;
    int j ;
  
    {
    i = 0;
    j = 10;
    {
    __loop_condition___0: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_0;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_0: /* CIL Label */ ;
    __loop_condition___1: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_1;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_1: /* CIL Label */ ;
    __loop_condition___2: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_2;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_2: /* CIL Label */ ;
    __loop_condition___3: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_3;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_3: /* CIL Label */ ;
    __loop_condition___4: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_4;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_4: /* CIL Label */ ;
    __loop_condition___5: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_5;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_5: /* CIL Label */ ;
    __loop_condition___6: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_6;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_6: /* CIL Label */ ;
    __loop_condition___7: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_7;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_7: /* CIL Label */ ;
    __loop_condition___8: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_8;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_8: /* CIL Label */ ;
    __loop_condition___9: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_9;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_9: /* CIL Label */ ;
    __loop_condition___10: /* CIL Label */ 
    if (! (i <= 10)) {
      goto loop_end;
    }
    if (i == 5) {
      i = 7;
      j = 3;
      goto loop_continue_10;
    }
    __goblint_check((_Bool )(i + j == 10));
    i ++;
    j --;
    loop_continue_10: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      __loop_condition: /* CIL Label */ 
      if (! (i <= 10)) {
        goto while_break;
      }
      if (i == 5) {
        i = 7;
        j = 3;
        goto while_continue;
      }
      __goblint_check((_Bool )(i + j == 10));
      i ++;
      j --;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    return (0);
  }
  }
