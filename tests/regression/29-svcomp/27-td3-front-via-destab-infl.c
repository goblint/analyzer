// NOMARSHAL PARAM: --set ana.activated[+] var_eq --set ana.activated[+] symb_locks --set ana.activated[+] region --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"

// creduced from ldv-linux-4.2-rc1/linux-4.2-rc1.tar.xz-43_2a-drivers--scsi--qla4xxx--qla4xxx.ko-entry_point.cil.out.i
// this program and params are really minimal, which produced a TD3 abort verify error, so don't simplify

int a;

void c();

void b() {
  c();
}

unsigned e() {
  int d;
  if (d)
    c();
  return;
}

void c() {
  int f;
  unsigned g = 0;

  do {
    g = e();
    if (a)
      return;

    do {
      f++;
    } while (f);

  } while (g);

  b();
}

int main() {
  b();
  return 0;
}
