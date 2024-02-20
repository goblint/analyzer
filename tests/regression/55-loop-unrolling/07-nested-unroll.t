  $ goblint --set lib.activated '["goblint"]' --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5 --enable justcil 07-nested-unroll.c
  [Info] unrolling loop at 07-nested-unroll.c:7:9-9:9 with factor 5
  [Info] unrolling loop at 07-nested-unroll.c:6:5-10:5 with factor 5
  [Info] unrolling loop at 07-nested-unroll.c:13:9-15:9 with factor 5
  [Info] unrolling loop at 07-nested-unroll.c:12:5-16:5 with factor 5
  extern void __goblint_check(int exp )  __attribute__((__goblint_array_domain__("unroll"))) ;
  extern void __goblint_assume(int exp ) ;
  extern void __goblint_assert(int exp ) ;
  extern void __goblint_assume_join() ;
  extern void __goblint_globalize(void *ptr ) ;
  extern void __goblint_split_begin(int exp ) ;
  extern void __goblint_split_end(int exp ) ;
  extern void __goblint_bounded(unsigned long long exp ) ;
  int main(void) 
  { 
    int arr[10][10]  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i  __attribute__((__goblint_array_domain__("unroll"))) ;
    int j  __attribute__((__goblint_array_domain__("unroll"))) ;
    int i___0  __attribute__((__goblint_array_domain__("unroll"))) ;
    int j___0  __attribute__((__goblint_array_domain__("unroll"))) ;
  
    {
    i = 0;
    {
    {
    {
    if (! (i < 10)) {
      goto loop_end___0;
    }
    j = 0;
    {
    {
    {
    if (! (j < 10)) {
      goto loop_end___1;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_0___1: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___1;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_1___0: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___1;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_2___0: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___1;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_3___0: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___1;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_4___0: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue: /* CIL Label */ ;
      if (! (j < 10)) {
        goto while_break;
      }
      arr[i][j] = i + j;
      j ++;
    }
    while_break: /* CIL Label */ ;
    }
    loop_end___1: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_0___0: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 10)) {
      goto loop_end___0;
    }
    j = 0;
    {
    {
    {
    if (! (j < 10)) {
      goto loop_end___2;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_0___2: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___2;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_1___2: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___2;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_2___1: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___2;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_3___1: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___2;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_4___1: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___0: /* CIL Label */ ;
      if (! (j < 10)) {
        goto while_break___0;
      }
      arr[i][j] = i + j;
      j ++;
    }
    while_break___0: /* CIL Label */ ;
    }
    loop_end___2: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_1___1: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 10)) {
      goto loop_end___0;
    }
    j = 0;
    {
    {
    {
    if (! (j < 10)) {
      goto loop_end___3;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_0___3: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___3;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_1___3: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___3;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_2___3: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___3;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_3___2: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___3;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_4___2: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___1: /* CIL Label */ ;
      if (! (j < 10)) {
        goto while_break___1;
      }
      arr[i][j] = i + j;
      j ++;
    }
    while_break___1: /* CIL Label */ ;
    }
    loop_end___3: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_2___2: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 10)) {
      goto loop_end___0;
    }
    j = 0;
    {
    {
    {
    if (! (j < 10)) {
      goto loop_end___4;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_0___4: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___4;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_1___4: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___4;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_2___4: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___4;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_3___4: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___4;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_4___3: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___2: /* CIL Label */ ;
      if (! (j < 10)) {
        goto while_break___2;
      }
      arr[i][j] = i + j;
      j ++;
    }
    while_break___2: /* CIL Label */ ;
    }
    loop_end___4: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_3___3: /* CIL Label */ ;
    }
    {
    {
    if (! (i < 10)) {
      goto loop_end___0;
    }
    j = 0;
    {
    {
    {
    if (! (j < 10)) {
      goto loop_end___5;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_0___5: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___5;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_1___5: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___5;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_2___5: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___5;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_3___5: /* CIL Label */ ;
    }
    {
    {
    if (! (j < 10)) {
      goto loop_end___5;
    }
    arr[i][j] = i + j;
    j ++;
    }
    loop_continue_4___5: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___3: /* CIL Label */ ;
      if (! (j < 10)) {
        goto while_break___3;
      }
      arr[i][j] = i + j;
      j ++;
    }
    while_break___3: /* CIL Label */ ;
    }
    loop_end___5: /* CIL Label */ ;
    }
    i ++;
    }
    loop_continue_4___4: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___4: /* CIL Label */ ;
      if (! (i < 10)) {
        goto while_break___4;
      }
      j = 0;
      {
      {
      {
      if (! (j < 10)) {
        goto loop_end;
      }
      arr[i][j] = i + j;
      j ++;
      }
      loop_continue_0: /* CIL Label */ ;
      }
      {
      {
      if (! (j < 10)) {
        goto loop_end;
      }
      arr[i][j] = i + j;
      j ++;
      }
      loop_continue_1: /* CIL Label */ ;
      }
      {
      {
      if (! (j < 10)) {
        goto loop_end;
      }
      arr[i][j] = i + j;
      j ++;
      }
      loop_continue_2: /* CIL Label */ ;
      }
      {
      {
      if (! (j < 10)) {
        goto loop_end;
      }
      arr[i][j] = i + j;
      j ++;
      }
      loop_continue_3: /* CIL Label */ ;
      }
      {
      {
      if (! (j < 10)) {
        goto loop_end;
      }
      arr[i][j] = i + j;
      j ++;
      }
      loop_continue_4: /* CIL Label */ ;
      }
      {
      while (1) {
        while_continue___5: /* CIL Label */ ;
        if (! (j < 10)) {
          goto while_break___5;
        }
        arr[i][j] = i + j;
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
    i___0 = 0;
    {
    {
    {
    if (! (i___0 < 5)) {
      goto loop_end___7;
    }
    j___0 = 0;
    {
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___8;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_0___8: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___8;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_1___7: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___8;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_2___7: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___8;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_3___7: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___8;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_4___7: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___6: /* CIL Label */ ;
      if (! (j___0 < 5)) {
        goto while_break___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
    }
    while_break___6: /* CIL Label */ ;
    }
    loop_end___8: /* CIL Label */ ;
    }
    i___0 ++;
    }
    loop_continue_0___7: /* CIL Label */ ;
    }
    {
    {
    if (! (i___0 < 5)) {
      goto loop_end___7;
    }
    j___0 = 0;
    {
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___9;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_0___9: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___9;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_1___9: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___9;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_2___8: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___9;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_3___8: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___9;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_4___8: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___7: /* CIL Label */ ;
      if (! (j___0 < 5)) {
        goto while_break___7;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
    }
    while_break___7: /* CIL Label */ ;
    }
    loop_end___9: /* CIL Label */ ;
    }
    i___0 ++;
    }
    loop_continue_1___8: /* CIL Label */ ;
    }
    {
    {
    if (! (i___0 < 5)) {
      goto loop_end___7;
    }
    j___0 = 0;
    {
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___10;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_0___10: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___10;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_1___10: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___10;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_2___10: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___10;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_3___9: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___10;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_4___9: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___8: /* CIL Label */ ;
      if (! (j___0 < 5)) {
        goto while_break___8;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
    }
    while_break___8: /* CIL Label */ ;
    }
    loop_end___10: /* CIL Label */ ;
    }
    i___0 ++;
    }
    loop_continue_2___9: /* CIL Label */ ;
    }
    {
    {
    if (! (i___0 < 5)) {
      goto loop_end___7;
    }
    j___0 = 0;
    {
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___11;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_0___11: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___11;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_1___11: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___11;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_2___11: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___11;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_3___11: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___11;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_4___10: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___9: /* CIL Label */ ;
      if (! (j___0 < 5)) {
        goto while_break___9;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
    }
    while_break___9: /* CIL Label */ ;
    }
    loop_end___11: /* CIL Label */ ;
    }
    i___0 ++;
    }
    loop_continue_3___10: /* CIL Label */ ;
    }
    {
    {
    if (! (i___0 < 5)) {
      goto loop_end___7;
    }
    j___0 = 0;
    {
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___12;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_0___12: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___12;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_1___12: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___12;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_2___12: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___12;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_3___12: /* CIL Label */ ;
    }
    {
    {
    if (! (j___0 < 5)) {
      goto loop_end___12;
    }
    __goblint_check(arr[i___0][j___0] == i___0 + j___0);
    j___0 ++;
    }
    loop_continue_4___12: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___10: /* CIL Label */ ;
      if (! (j___0 < 5)) {
        goto while_break___10;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
    }
    while_break___10: /* CIL Label */ ;
    }
    loop_end___12: /* CIL Label */ ;
    }
    i___0 ++;
    }
    loop_continue_4___11: /* CIL Label */ ;
    }
    {
    while (1) {
      while_continue___11: /* CIL Label */ ;
      if (! (i___0 < 5)) {
        goto while_break___11;
      }
      j___0 = 0;
      {
      {
      {
      if (! (j___0 < 5)) {
        goto loop_end___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
      }
      loop_continue_0___6: /* CIL Label */ ;
      }
      {
      {
      if (! (j___0 < 5)) {
        goto loop_end___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
      }
      loop_continue_1___6: /* CIL Label */ ;
      }
      {
      {
      if (! (j___0 < 5)) {
        goto loop_end___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
      }
      loop_continue_2___6: /* CIL Label */ ;
      }
      {
      {
      if (! (j___0 < 5)) {
        goto loop_end___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
      }
      loop_continue_3___6: /* CIL Label */ ;
      }
      {
      {
      if (! (j___0 < 5)) {
        goto loop_end___6;
      }
      __goblint_check(arr[i___0][j___0] == i___0 + j___0);
      j___0 ++;
      }
      loop_continue_4___6: /* CIL Label */ ;
      }
      {
      while (1) {
        while_continue___12: /* CIL Label */ ;
        if (! (j___0 < 5)) {
          goto while_break___12;
        }
        __goblint_check(arr[i___0][j___0] == i___0 + j___0);
        j___0 ++;
      }
      while_break___12: /* CIL Label */ ;
      }
      loop_end___6: /* CIL Label */ ;
      }
      i___0 ++;
    }
    while_break___11: /* CIL Label */ ;
    }
    loop_end___7: /* CIL Label */ ;
    }
    return (0);
  }
  }
