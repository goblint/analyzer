//PARAM: --set ana.modular.funs "['getndelim2']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
struct {
  char *a;
} b, c;
long e;
const char* d() { return b.a; }
f() { c.a = e; }
freadseek() { f(); }
getndelim2() {
  const char* buffer;
  while (1) {
    buffer = d();
    if (buffer)
      ;
    else
      goto g;
    freadseek();
  }
g:
}
