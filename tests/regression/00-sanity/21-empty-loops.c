int main()
{
  // non-deterministically make all variants live
  int r;
  switch (r)
  {
    case 0:
      f_empty_goto_loop();
      break;
    case 1:
      f_empty_while_loop();
      break;
    case 2:
      f_empty_goto_loop_suffix();
      break;
    case 3:
      f_empty_while_loop_suffix();
      break;
    case 4:
      f_nonempty_goto_loop();
      break;
    case 5:
      f_nonempty_while_loop();
      break;
  }

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