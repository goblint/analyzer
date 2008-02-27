int main()
{
  int x, y;
  int *p = &x;
  ++ p;
  assert(p == &y); // UNKNOWN
  return 0;
}
