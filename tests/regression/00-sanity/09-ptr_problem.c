int main()
{
  int x, y;
  int *p = &x;
  ++ p;
  return p == &y;
}
