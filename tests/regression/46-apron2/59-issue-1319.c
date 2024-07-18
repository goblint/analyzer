// SKIP PARAM: --enable ana.int.def_exc --enable ana.int.interval --set ana.activated[+] apron
int other();

int main()
{
  unsigned char *t;
  char c = 'b';

  t = &c;

  // Type of *t and c do not match, this caused a crash before
  if(*t == 'a') {
    t++;
  }

  other();
}

int other()
{
  // Same problem, but a bit more involved
  unsigned char *t;
  char buf[100] = "bliblablubapk\r";

  t = buf;

  if(*t == 'a') {
    t++;
  }
}