// SKIP PARAM: --set ana.activated[+] var_eq
// manually minimized from sv-benchmarks/c/ldv-linux-3.16-rc1/205_9a_array_unsafes_linux-3.16-rc1.tar.xz-205_9a-drivers--net--usb--cx82310_eth.ko-entry_point.cil.out.i
// used to call widen incorrectly
typedef _Bool bool;

void usb_bulk_msg(int *arg4, int x) {

}

void cx82310_cmd(bool reply ,unsigned char *wdata , int wlen)
{
  int actual_len ;
  int retries ;
  int __min1 ;
  __min1 = wlen;

  retries = 0;
  while (retries <= 4) {
    usb_bulk_msg(&actual_len, 1U);
    if (actual_len > 0)
      return;
    retries = retries + 1;
  }
}

int main(void)
{
  cx82310_cmd(1, "a", 1);
  return 0;
}
