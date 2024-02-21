  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5 --enable justcil 06-simple-cases-unrolled.c
  [Info] unrolling loop at 06-simple-cases-unrolled.c:27:5-30:5 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:42:5-45:19 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:57:5-60:5 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:74:5-80:5 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:95:5-105:5 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:119:5-123:5 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:143:2-146:2 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:160:9-163:9 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:157:2-165:2 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:174:2-178:2 with factor 5
  [Info] unrolling loop at 06-simple-cases-unrolled.c:187:2-194:2 with factor 5
  extern void __goblint_check(int exp )  __attribute__((__goblint_array_domain__("unroll"))) ;
  extern void __goblint_assume(int exp ) ;
  extern void __goblint_assert(int exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  int global  ;
  void example1(void) ;
  void example2(void) ;
  void example3(void) ;
  void example4(void) ;
  void example5(void) ;
  void example6(void) ;
  void example7(void) ;
  void example8(void) ;
  void example9(void) ;
  void example10(void) ;
  int main(void) 
  { 
  
  
    {
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();
    example10();
    return (0);
  }
  }
  void example1(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
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
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 3);
    return;
  }
  }
  void example2(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    a[i] = i;
    i ++;
    if (! (i <= 5)) {
      goto loop_end;
    }
    }
    loop_continue_0: /* CIL Label */ ;
    {
    a[i] = i;
    i ++;
    if (! (i <= 5)) {
      goto loop_end;
    }
    }
    loop_continue_1: /* CIL Label */ ;
    {
    a[i] = i;
    i ++;
    if (! (i <= 5)) {
      goto loop_end;
    }
    }
    loop_continue_2: /* CIL Label */ ;
    {
    a[i] = i;
    i ++;
    if (! (i <= 5)) {
      goto loop_end;
    }
    }
    loop_continue_3: /* CIL Label */ ;
    {
    a[i] = i;
    i ++;
    if (! (i <= 5)) {
      goto loop_end;
    }
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      a[i] = i;
      i ++;
      if (! (i <= 5)) {
        goto while_break;
      }
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 3);
    return;
  }
  }
  void example3(void) 
  { 
    int a[10]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
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
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 0);
    __goblint_check(a[7] == 0);
    return;
  }
  }
  void example4(void) 
  { 
    int a[10]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int first_iteration  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    first_iteration = 1;
    {
    {
    if (! (i < 10)) {
      goto loop_end;
    }
    if (first_iteration == 1) {
      __goblint_check(i == 0);
    } else
    if (i > 5) {
      __goblint_check(i == 6);
    }
    first_iteration = 0;
    a[i] = 0;
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 10)) {
      goto loop_end;
    }
    if (first_iteration == 1) {
      __goblint_check(i == 0);
    } else
    if (i > 5) {
      __goblint_check(i == 6);
    }
    first_iteration = 0;
    a[i] = 0;
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 10)) {
      goto loop_end;
    }
    if (first_iteration == 1) {
      __goblint_check(i == 0);
    } else
    if (i > 5) {
      __goblint_check(i == 6);
    }
    first_iteration = 0;
    a[i] = 0;
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 10)) {
      goto loop_end;
    }
    if (first_iteration == 1) {
      __goblint_check(i == 0);
    } else
    if (i > 5) {
      __goblint_check(i == 6);
    }
    first_iteration = 0;
    a[i] = 0;
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 10)) {
      goto loop_end;
    }
    if (first_iteration == 1) {
      __goblint_check(i == 0);
    } else
    if (i > 5) {
      __goblint_check(i == 6);
    }
    first_iteration = 0;
    a[i] = 0;
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 10)) {
        goto while_break;
      }
      if (first_iteration == 1) {
        __goblint_check(i == 0);
      } else
      if (i > 5) {
        __goblint_check(i == 6);
      }
      first_iteration = 0;
      a[i] = 0;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(first_iteration == 0);
    return;
  }
  }
  void example5(void) 
  { 
    int a[4]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int top  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    top = 0;
    {
    {
    if (! (i < 4)) {
      goto loop_end;
    }
    a[i] = 0;
    top += i;
    if (i == 2) {
      __goblint_check(top == 3);
    } else {
      __goblint_check(top == 3);
    }
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 4)) {
      goto loop_end;
    }
    a[i] = 0;
    top += i;
    if (i == 2) {
      __goblint_check(top == 3);
    } else {
      __goblint_check(top == 3);
    }
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 4)) {
      goto loop_end;
    }
    a[i] = 0;
    top += i;
    if (i == 2) {
      __goblint_check(top == 3);
    } else {
      __goblint_check(top == 3);
    }
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 4)) {
      goto loop_end;
    }
    a[i] = 0;
    top += i;
    if (i == 2) {
      __goblint_check(top == 3);
    } else {
      __goblint_check(top == 3);
    }
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 4)) {
      goto loop_end;
    }
    a[i] = 0;
    top += i;
    if (i == 2) {
      __goblint_check(top == 3);
    } else {
      __goblint_check(top == 3);
    }
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 4)) {
        goto while_break;
      }
      a[i] = 0;
      top += i;
      if (i == 2) {
        __goblint_check(top == 3);
      } else {
        __goblint_check(top == 3);
      }
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 0);
    __goblint_check(top == 6);
    return;
  }
  }
  void example6(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int top ;
  
    {
    i = 0;
    top = 0;
    {
    {
    if (! (i < 3)) {
      goto loop_end;
    }
    a[i] = 0;
    __goblint_check(a[0] == 0);
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 3)) {
      goto loop_end;
    }
    a[i] = 0;
    __goblint_check(a[0] == 0);
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 3)) {
      goto loop_end;
    }
    a[i] = 0;
    __goblint_check(a[0] == 0);
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 3)) {
      goto loop_end;
    }
    a[i] = 0;
    __goblint_check(a[0] == 0);
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 3)) {
      goto loop_end;
    }
    a[i] = 0;
    __goblint_check(a[0] == 0);
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 3)) {
        goto while_break;
      }
      a[i] = 0;
      __goblint_check(a[0] == 0);
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 0);
    __goblint_check(top == 6);
    return;
  }
  }
  int update(int i )  __attribute__((__goblint_array_domain__("unroll"))) ;
  int update(int i ) 
  { 
  
  
    {
    if (i > 5) {
      return (0);
    } else {
      return (1);
    }
  }
  }
  void example7(void) 
  { 
    int a[10]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int tmp  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    tmp = update(i);
    if (! tmp) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    tmp = update(i);
    if (! tmp) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    tmp = update(i);
    if (! tmp) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    tmp = update(i);
    if (! tmp) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    tmp = update(i);
    if (! tmp) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      tmp = update(i);
      if (! tmp) {
        goto while_break;
      }
      a[i] = i;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    __goblint_check(a[0] == 0);
    __goblint_check(a[6] == 0);
    return;
  }
  }
  void example8(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int b[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int j  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    b[0] = 0;
    b[1] = 0;
    b[2] = 0;
    b[3] = 0;
    b[4] = 0;
    i = 0;
    {
    {
    if (! (i < 5)) {
      goto loop_end___0;
    }
    a[i] = i;
    j = 0;
    {
    {
    if (! (j < 5)) {
      goto loop_end___1;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_0___1: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___1;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_1___0: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___1;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_2___0: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___1;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_3___0: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___1;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_4___0: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (j < 5)) {
        goto while_break;
      }
      b[j] += a[i];
      j ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end___1: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_0___0: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end___0;
    }
    a[i] = i;
    j = 0;
    {
    {
    if (! (j < 5)) {
      goto loop_end___2;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_0___2: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___2;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_1___2: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___2;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_2___1: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___2;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_3___1: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___2;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_4___1: /* CIL Label */ ;
    {
    while (1) {
      while_continue___0: /* CIL Label */ ;
      if (! (j < 5)) {
        goto while_break___0;
      }
      b[j] += a[i];
      j ++;
    }
    while_break___0: /* CIL Label */ ;
    }
    loop_end___2: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_1___1: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end___0;
    }
    a[i] = i;
    j = 0;
    {
    {
    if (! (j < 5)) {
      goto loop_end___3;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_0___3: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___3;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_1___3: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___3;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_2___3: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___3;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_3___2: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___3;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_4___2: /* CIL Label */ ;
    {
    while (1) {
      while_continue___1: /* CIL Label */ ;
      if (! (j < 5)) {
        goto while_break___1;
      }
      b[j] += a[i];
      j ++;
    }
    while_break___1: /* CIL Label */ ;
    }
    loop_end___3: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_2___2: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end___0;
    }
    a[i] = i;
    j = 0;
    {
    {
    if (! (j < 5)) {
      goto loop_end___4;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_0___4: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___4;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_1___4: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___4;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_2___4: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___4;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_3___4: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___4;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_4___3: /* CIL Label */ ;
    {
    while (1) {
      while_continue___2: /* CIL Label */ ;
      if (! (j < 5)) {
        goto while_break___2;
      }
      b[j] += a[i];
      j ++;
    }
    while_break___2: /* CIL Label */ ;
    }
    loop_end___4: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_3___3: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end___0;
    }
    a[i] = i;
    j = 0;
    {
    {
    if (! (j < 5)) {
      goto loop_end___5;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_0___5: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___5;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_1___5: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___5;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_2___5: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___5;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_3___5: /* CIL Label */ ;
    {
    if (! (j < 5)) {
      goto loop_end___5;
    }
    b[j] += a[i];
    j ++;
    }
    loop_continue_4___5: /* CIL Label */ ;
    {
    while (1) {
      while_continue___3: /* CIL Label */ ;
      if (! (j < 5)) {
        goto while_break___3;
      }
      b[j] += a[i];
      j ++;
    }
    while_break___3: /* CIL Label */ ;
    }
    loop_end___5: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_4___4: /* CIL Label */ ;
    {
    while (1) {
      while_continue___4: /* CIL Label */ ;
      if (! (i < 5)) {
        goto while_break___4;
      }
      a[i] = i;
      j = 0;
      {
      {
      if (! (j < 5)) {
        goto loop_end;
      }
      b[j] += a[i];
      j ++;
      }
      loop_continue_0: /* CIL Label */ ;
      {
      if (! (j < 5)) {
        goto loop_end;
      }
      b[j] += a[i];
      j ++;
      }
      loop_continue_1: /* CIL Label */ ;
      {
      if (! (j < 5)) {
        goto loop_end;
      }
      b[j] += a[i];
      j ++;
      }
      loop_continue_2: /* CIL Label */ ;
      {
      if (! (j < 5)) {
        goto loop_end;
      }
      b[j] += a[i];
      j ++;
      }
      loop_continue_3: /* CIL Label */ ;
      {
      if (! (j < 5)) {
        goto loop_end;
      }
      b[j] += a[i];
      j ++;
      }
      loop_continue_4: /* CIL Label */ ;
      {
      while (1) {
        while_continue___5: /* CIL Label */ ;
        if (! (j < 5)) {
          goto while_break___5;
        }
        b[j] += a[i];
        j ++;
      }
      while_break___5: /* CIL Label */ ;
      }
      loop_end: /* CIL Label */ ;
      }
      i ++;
    }
    while_break___4: /* CIL Label */ ;
    }
    loop_end___0: /* CIL Label */ ;
    }
    return;
  }
  }
  void example9(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    if (! 1) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    if (i == 5) {
      goto loop_end;
    }
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! 1) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    if (i == 5) {
      goto loop_end;
    }
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! 1) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    if (i == 5) {
      goto loop_end;
    }
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! 1) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    if (i == 5) {
      goto loop_end;
    }
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! 1) {
      goto loop_end;
    }
    a[i] = i;
    i ++;
    if (i == 5) {
      goto loop_end;
    }
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! 1) {
        goto while_break;
      }
      a[i] = i;
      i ++;
      if (i == 5) {
        goto while_break;
      }
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    return;
  }
  }
  void example10(void) 
  { 
    int a[5]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    if (i == 3) {
      i ++;
      goto loop_continue_0;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_0: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    if (i == 3) {
      i ++;
      goto loop_continue_1;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_1: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    if (i == 3) {
      i ++;
      goto loop_continue_2;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_2: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    if (i == 3) {
      i ++;
      goto loop_continue_3;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_3: /* CIL Label */ ;
    {
    if (! (i < 5)) {
      goto loop_end;
    }
    if (i == 3) {
      i ++;
      goto loop_continue_4;
    }
    a[i] = i;
    i ++;
    }
    loop_continue_4: /* CIL Label */ ;
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (i < 5)) {
        goto while_break;
      }
      if (i == 3) {
        i ++;
        goto while_continue;
      }
      a[i] = i;
      i ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end: /* CIL Label */ ;
    }
    return;
  }
  }
