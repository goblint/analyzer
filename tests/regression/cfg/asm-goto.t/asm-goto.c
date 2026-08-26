int main() {
label0: // This is actually unused
  asm goto (
    "rdrand %%eax\n"
    "and $1, %%eax\n"
    "test %%eax, %%eax\n"
    "jz %l0\n"
    "jmp %l1"
    :
    :
    : "rax", "cc"
    : label1, label3, label0
  );
  return 0;
label1:
  return 1; // This is reachable
label2:
  return 2; // This is certainly dead!
label3:
  return 3; // This is reachable
}
