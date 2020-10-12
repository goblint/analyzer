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

void suffix()
{

}

void f_empty_goto_loop_suffix()
{
f_empty_goto_loop_suffix_label:
  goto f_empty_goto_loop_suffix_label;

  suffix();
}

void f_empty_while_loop_suffix()
{
  while (1) {}

  suffix();
}

void body()
{

}

void f_nonempty_goto_loop()
{
f_nonempty_goto_loop_label:
  body();
  goto f_nonempty_goto_loop_label;
}

void f_nonempty_while_loop()
{
  while (1)
  {
    body();
  }
}