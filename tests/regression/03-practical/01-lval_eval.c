extern unsigned short const   **__ctype_b_loc(void)  __attribute__((__const__)) ;

int main(void) { 
  int i;
  unsigned short const   **tmp ;

  tmp = __ctype_b_loc();
  i = (int)(*((*tmp) + 13));
  return 0;
}
