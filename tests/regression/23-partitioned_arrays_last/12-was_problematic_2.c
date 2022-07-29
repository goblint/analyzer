// PARAM: --set solver td3 --enable ana.int.interval  --set ana.base.arrays.domain partitioned --set ana.base.partition-arrays.keep-expr "last"
int main(void)
{
  int arr[260];
  int n;

  n = 5;
  arr[0] = 0;

  while (n > 1) { //here
    arr[1] = 7;
    n--;
  }

  return 0;
}
