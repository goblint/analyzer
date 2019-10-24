extern void __VERIFIER_error() __attribute__((__noreturn__));

int main()
{
  f_empty_goto_loop();
  f_empty_while_loop();
  f_empty_goto_loop_suffix();
  f_empty_while_loop_suffix();
  f_nonempty_goto_loop();
  f_nonempty_while_loop();

  return 0;
}

void f_empty_goto_loop()
{
f_empty_goto_loop_label:
  goto f_empty_goto_loop_label;
}

void f_empty_while_loop()
{
  while (1) {}
}

void f_empty_goto_loop_suffix()
{
f_empty_goto_loop_suffix_label:
  goto f_empty_goto_loop_suffix_label;

  __VERIFIER_error();
}

void f_empty_while_loop_suffix()
{
  while (1) {}

  __VERIFIER_error();
}

void f_nonempty_goto_loop()
{
f_nonempty_goto_loop_label:
  __VERIFIER_error();
  goto f_nonempty_goto_loop_label;
}

void f_nonempty_while_loop()
{
  while (1)
  {
    __VERIFIER_error();
  }
}