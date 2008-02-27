// This function sets all varargs to the given x
void maja (int x, ...) {
  va_list args;
  int *i;
  va_start(args, x);
  i = va_arg(args, int*);
  va_end(args);
  *i = x;
  return;
}

