// CRAM

int main()
{
  int x;
  while (1) {
    asm goto (
      "nop"
      :
      :
      :
      : label1, label2
    );
    x = 0;
    continue;
  label1:
    x = 1;
  }
  return 0;
label2:
  return 1;
}
