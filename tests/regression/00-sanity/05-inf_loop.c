int main()
{
  while (1);
  __goblint_check(0); // NOWARN (unreachable), formerly NONTERM
  return 0;
}
