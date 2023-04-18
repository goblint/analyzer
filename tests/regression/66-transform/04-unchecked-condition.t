  $ ./transform.sh remove_dead_code -- 04-unchecked-condition.c
  // dead code removal transformation: conditions that check this variable are dead
  int _UNCHECKED_CONDITION_1  ;
  int _UNCHECKED_CONDITION  =    3;
  int f_both(int x ) 
  { 
    int result ;
  
    {
    if (x > 7) {
      goto true_block;
    } else {
      goto false_block;
    }
    if (_UNCHECKED_CONDITION_1) {
      true_block: 
      {
      result = 2;
      }
    } else {
      false_block: 
      {
      result = 12;
      }
    }
    return (result);
  }
  }
  int f_true(int x ) 
  { 
    int result ;
  
    {
    if (x > 7) {
      goto true_block;
    }
    {
    true_block: 
    {
    result = 2;
    }
    }
    return (result);
  }
  }
  int f_false(int x ) 
  { 
    int result ;
  
    {
    if (! (x > 7)) {
      goto false_block;
    }
    {
    false_block: 
    {
    result = 12;
    }
    }
    return (result);
  }
  }
  int conflicting_local(void) 
  { 
    int _UNCHECKED_CONDITION_1___0 ;
  
    {
    {
    _UNCHECKED_CONDITION_1___0 = 2;
    }
    return (_UNCHECKED_CONDITION_1___0);
  }
  }
  int main(void) 
  { 
  
  
    {
    {
    f_both(3);
    f_both(9);
    f_true(12);
    f_false(-3);
    conflicting_local();
    }
    if (_UNCHECKED_CONDITION) {
      return (2);
    }
  }
  }
