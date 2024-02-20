  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5 --enable justcil 04-simple.c
  [Info] unrolling loop at 04-simple.c:10:5-13:5 with factor 5
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
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_0: ;
    }
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_1: ;
    }
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_2: ;
    }
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_3: ;
    }
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_4: ;
    }
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 5)) {
        goto while_break;
      }
      a[i] = i;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 3);
    return;
  }
  }
