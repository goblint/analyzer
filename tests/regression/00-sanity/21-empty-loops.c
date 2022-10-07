// CRAM
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
    case 6:
      f_empty_goto_loop_prefix();
      break;
    case 7:
      f_empty_while_loop_prefix();
      break;
    case 8:
      f_empty_goto_loop_semicolon();
      break;
    case 9:
      f_empty_while_loop_semicolon();
      break;
    case 10:
      f_empty_goto_loop_multiple();
      break;
    case 11:
      f_empty_goto_loop_multiple_semicolon_first();
      break;
    case 12:
      f_empty_goto_loop_multiple_semicolon_second();
      break;
    case 13:
      f_empty_goto_loop_multiple_semicolon_both();
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

void prefix()
{

}

void f_empty_goto_loop_prefix()
{
  prefix();

f_empty_goto_loop_prefix_label:
  goto f_empty_goto_loop_prefix_label;
}

void f_empty_while_loop_prefix()
{
  prefix();

  while (1) {}
}

void f_empty_goto_loop_semicolon()
{
f_empty_goto_loop_semicolon_label:
  ; // this semicolon makes a difference!
  // CIL now puts the Goto into a Block and they are in a mutual successor loop
  goto f_empty_goto_loop_semicolon_label;
}

void f_empty_while_loop_semicolon()
{
  while (1) {
    ; // this semicolon doesn't make a difference
  }
}

void f_empty_goto_loop_multiple()
{
f_empty_goto_loop_multiple_label_1:
  goto f_empty_goto_loop_multiple_label_2;
f_empty_goto_loop_multiple_label_2:
  goto f_empty_goto_loop_multiple_label_1;
}

void f_empty_goto_loop_multiple_semicolon_first()
{
f_empty_goto_loop_multiple_semicolon_first_label_1:
  ; // this semicolon makes a difference!
  goto f_empty_goto_loop_multiple_semicolon_first_label_2;
f_empty_goto_loop_multiple_semicolon_first_label_2:
  goto f_empty_goto_loop_multiple_semicolon_first_label_1;
}

void f_empty_goto_loop_multiple_semicolon_second()
{
f_empty_goto_loop_multiple_semicolon_second_label_1:
  goto f_empty_goto_loop_multiple_semicolon_second_label_2;
f_empty_goto_loop_multiple_semicolon_second_label_2:
  ; // this semicolon makes a difference!
  goto f_empty_goto_loop_multiple_semicolon_second_label_1;
}

void f_empty_goto_loop_multiple_semicolon_both()
{
f_empty_goto_loop_multiple_semicolon_both_label_1:
  ; // this semicolon makes a difference!
  goto f_empty_goto_loop_multiple_semicolon_both_label_2;
f_empty_goto_loop_multiple_semicolon_both_label_2:
  ; // this semicolon makes a difference!
  goto f_empty_goto_loop_multiple_semicolon_both_label_1;
}
