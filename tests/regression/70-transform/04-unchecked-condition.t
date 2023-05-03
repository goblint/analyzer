  $ ./transform.sh remove_dead_code -- 04-unchecked-condition.c
  int f_both(int x ) 
  { 
    int result ;
  
    {
    if (x > 7) {
      goto true_block;
    } else {
      goto false_block;
    }
    if ("UNCHECKED CONDITION") {
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
  int main(void) 
  { 
  
  
    {
    {
    f_both(3);
    f_both(9);
    f_true(12);
    f_false(-3);
    }
    return (0);
  }
  }
