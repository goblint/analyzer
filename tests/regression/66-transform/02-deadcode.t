  $ ./transform.sh remove_dead_code -- --enable ana.int.interval --enable sem.noreturn.dead_code 02-deadcode.c | clang-format
  extern void abort() __attribute__((__noreturn__));
  int basic1(int n) {
    int a;
    int b;
    int c;
    int i;
  
    {
      {
        a = 0;
        b = 1;
      }
      if (n < 0) {
      }
      { i = 0; }
      {
        while (1) {
  
          if (!(i < n)) {
            goto while_break;
          }
          {
            c = a + b;
            a = b;
            b = c;
            i++;
          }
        }
      while_break: /* CIL Label */;
      }
      return (a);
    }
  }
  int basic2(int n) {
    int a;
    int i;
  
    {
      if (n < 0) {
        return (0);
      }
      {
        while (1) {
  
          if (!(i < n)) {
            goto while_break;
          }
          { a += i + n; }
        }
      while_break: /* CIL Label */;
      }
    }
  }
  int one_branch_dead(int x) {
  
    {
      if (x > 7) {
        return (x);
      }
    }
  }
  int uncalled_function(int x) {
    int y;
  
    {
      { y = x + 1; }
      return (x * y);
    }
  }
  int conditional_call_in_loop(int x) {
    int i;
  
    {
      { i = 0; }
      {
        while (1) {
  
          if (!(i < x)) {
            goto while_break;
          }
          if (i > 7) {
          }
          { i++; }
        }
      while_break: /* CIL Label */;
      }
      return (0);
    }
  }
  int compound_statement_in_out(void) {
  
    {
      goto in;
      if (1) {
      in:
        goto out;
      }
    out:
      return;
    }
  }
  int main(void) {
    int (*f)(int);
  
    {
      {
        basic1(7);
        basic1(3);
        basic2(-3);
        basic2(-6);
        one_branch_dead(9);
        f = &uncalled_function;
        conditional_call_in_loop(5);
        compound_statement_in_out();
        abort();
      }
    }
  }
