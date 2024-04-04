int main() {
  int i = 0;
  while (i < 10)
    i++;

  int j = 0, k = 0;
  while (j < 10) {
    while (k < 100)
      k++;
    j++;
  }
  return 0;
}
