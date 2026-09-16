  $ ./transform.sh remove_dead_code -- --enable ana.int.interval --enable sem.noreturn.dead_code 05-asm-goto.c
  int main(void) 
  { 
  
  
    {
    __asm__  goto   ("rdrand %%eax\n"
                     "and $1, %%eax\n"
                     "test %%eax, %%eax\n"
                     "jz %l0\n"
                     "jmp %l1": : : "rax", "cc": label1, label3);
    return (0);
    label1: 
    return (1);
    label3: 
    return (3);
  }
  }
