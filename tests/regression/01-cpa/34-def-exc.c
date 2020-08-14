// --enable ana.int.def_exc --disable ana.int.interval
void main()
{
  char yy[256];
  char *ryy_j = &yy[1];

  int i =0;

  ryy_j++;
  ryy_j++;

  while(i < 2) { // Here was an issue with a fixpoint not being reached
    ryy_j = ryy_j + 1;
    i++;
  }

  // The type of ! needs to be IInt
  long l;
  int r = !l + 4;

  // From dtlk driver example, produces
  // "warning: pointer targets in assignment differ in signedness [-Wpointer-sign]"
  // in GCC
	unsigned char *t;
	static char buf[100] = "bliblablubapk\r";

	t = buf;
	t += 2;

  *t = '\r';
	while (*t != '\r') {
    t++;
  }

  unsigned int v;
  unsigned short s1, s2;

  v = v & 0xFFF0FFFF;
  v = v << 16;

  v = (unsigned int)(((unsigned int) (s1 ^ s2)));
  v = (unsigned int)(((unsigned int) (s1 ^ s2)) << (16));

  v = (v & 0xFFF0FFFF) |
	    (((unsigned int) s1 ^ s2) << 16);

  return;
}
