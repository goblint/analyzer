  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 2 --enable justcil 09-weird.c
  [Info] unrolling loop at 09-weird.c:8:5-11:5 with factor 2
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
    goto somelab___0;
    somelab___0: 
    j = 8;
    i ++;
    }
    loop_continue_0: ;
    }
    {
    {
    if (! (i < 50)) {
      goto loop_end;
    }
    goto somelab___1;
    somelab___1: 
    j = 8;
    i ++;
    }
    loop_continue_1: ;
    }
    {
    while (1) {
      while_continue: /* CIL Label */ ;
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
    loop_end: ;
    }
    __goblint_check(j == 8);
    return;
  }
  }
