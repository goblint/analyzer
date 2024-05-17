struct a {
  int (*c)();
} q, *r[] = {&q};
struct d {
  char f;
};
union g {
  int h;
  struct d i;
};
struct j {
  union g data;
} * aa;
struct k {
  struct j e;
} * w;
struct l {
  struct k *m;
} * ab;
int n, y;
char s;
void *memset();
void *malloc();
void o();
void main() { o(); }
int(pthread_create)();
void c();
void o() { c(); }
void c() {
  struct a b = **r;
  b.c(s, n);
}
void t();
int u(char *z, int ac) { t(); }
struct a q = {u};
void v(struct j *z) {
  if (z->data.i.f)
    ;
}
void x(struct k **z, struct j *ac) {
  void *ae = malloc(sizeof(struct k));
  w = ae;
  w->e = *ac;
  *z = ae;
}
void af() {
  while (!y) {
    void *ae = malloc(sizeof(struct l)), *ag = malloc(sizeof(struct j));
    ab = ae;
    aa = ag;
    memset(aa, 0, sizeof(struct j));
    x(&ab->m, aa);
    struct l p = *ab;
    struct k *ad = p.m;
    v(&ad->e);
  }
}
void t() { long ah = pthread_create(&ah, 0, af, 0); }
