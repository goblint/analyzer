// PARAM: --set ana.activated "['base','term']" --enable dbg.debug --enable ana.int.interval --sets solver slr3

int main(){
    int i = 0;
    while (i < 5) {
        i++;
    }
//#line 5
//  while (1) {
//    while_continue: /* CIL Label */ ;
//#line 5
//    if (! (i < 5)) {
//#line 5
//      goto while_break;
//    }
//    {
//#line 6
//    i ++;
//    }
//  }
//  while_break: /* CIL Label */ ;
    while (1) {
        if(i >= 5) break;
        i++;
    }
//#line 22
//  while (1) {
//    while_continue___0: /* CIL Label */ ;
//#line 23
//    if (i >= 5) {
//#line 23
//      goto while_break___0;
//    }
//    {
//#line 24
//    i ++;
//    }
//  }
//  while_break___0: /* CIL Label */ ;
    while (1) {
        i++;
        if(i >= 5) break;
    }
//#line 40
//  while (1) {
//    while_continue___1: /* CIL Label */ ;
//    {
//#line 41
//    i ++;
//    }
//#line 42
//    if (i >= 5) {
//#line 42
//      goto while_break___1;
//    }
//  }
//  while_break___1: /* CIL Label */ ;
    for (i=0; i<5; i++) {
    }
//  {
//#line 58
//  i = 0;
//  }
//  {
//#line 58
//  while (1) {
//    while_continue___2: /* CIL Label */ ;
//#line 58
//    if (! (i < 5)) {
//#line 58
//      goto while_break___2;
//    }
//    {
//#line 58
//    i ++;
//    }
//  }
//  while_break___2: /* CIL Label */ ;
//  }
    while (1) {
        if(i >= 5) return 1;
        i++;
    }
//#line 80
//  while (1) {
//    while_continue___3: /* CIL Label */ ;
//#line 81
//    if (i >= 5) {
//#line 81
//      return (1);
//    }
//    {
//#line 82
//    i ++;
//    }
//  }
//  while_break___3: /* CIL Label */ ;
    while (1) {
        i++;
    }
//    {
//#line 99
//    while (1) {
//      while_continue___4: /* CIL Label */ ;
//      {
//#line 100
//      i ++;
//      }
//    }
//    while_break___4: /* CIL Label */ ;
//    }
    return 0;
}
