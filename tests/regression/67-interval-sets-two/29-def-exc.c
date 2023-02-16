// PARAM: --enable ana.int.def_exc --enable ana.int.interval_set
#include <goblint.h>
#define LONGS(x) (((x) + sizeof(unsigned long) - 1)/sizeof(unsigned long))
#include<stdbool.h>

union U1 {
   unsigned char f0;
   int f1;
};


typedef unsigned long custom_t;
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

  int tmp___1;
  int *tmp___2;

  _Bool  fclose_fail = (_Bool )(tmp___1 != 0);

  if (! fclose_fail) {
    *tmp___2 = 0;
  }

  hash_initialize();



  custom_t ci;
  void const* b = (void const*) ci;
  test((void const *)ci);

	for (i = 0; i <	 (((20) + (4) - 1)/(4)); i++) {

	}


  for (i = 0; i <	 (((20) + (8UL) - 1)/(8UL)); i++) {

	}




  unsigned long v = 8UL;
  for (i = 0; i <	 (((20) + (8UL) - 1)/(v)); i++) {

	}

  i = 0;
  if(i < 28UL/v) {
    i++;
  }

  int gef = 75;

  if(i < 28UL/v) {
    i++;
  }

  gef = 38;

  i =0;
  for(; i < 28UL/v; i++) {

  }

	for (i = 0; i <	 (((20) + sizeof(unsigned long) - 1)/sizeof(unsigned long)); i++) {

	}

	for (i = 0; i <	 LONGS(20); i++) {

	}

  int top;

  union U1 g_76;
  g_76.f0 = 12; // (f0, (`Definite 12, [0,8]))
  if (top) {
    g_76.f1 = 5; // (f1, (`Definite 5, [-31,31]))
  }


  unsigned char v;
  signed char u;
  int r2 = ((int )v == (int )u);


  signed char l_48 =  (signed char )58L;
  unsigned char l_67 = (unsigned char)48UL;

  signed char *l_247 ;
  unsigned char *l_229 ;

  l_247 = &l_48;

  if (top) {
      l_229 = &l_67;
      l_247 = (signed char *)l_229;
  }

  signed char res = *l_247;

  signed short x;
  unsigned int y;
  unsigned short z = 0x7ED9L;

  if (((((((signed char)y) == x) && z))))
  {

  }

  return;
}


struct hash_table {
  custom_t n_buckets ;
};

typedef struct hash_table Hash_table;

Hash_table *hash_initialize()
{ Hash_table *table___0 ;
  void *tmp ;
  _Bool tmp___0 ;
  void *tmp___1 ;

  tmp = malloc(sizeof(*table___0));

  custom_t n;
  table___0 = (Hash_table *)tmp;
  table___0->n_buckets = n;

  if (! table___0->n_buckets) {

    goto fail;
  }
  fail:

  free((void *)table___0);

  return ((Hash_table *)((void *)0));
}

int test(void const   *ptr) {
  if(!ptr) {
    __goblint_check(ptr == 0);
    int f = 7;
  } else {
    __goblint_check(ptr == 1); //UNKNOWN!
    __goblint_check(ptr != 0);
    int f= 38;
  }
}
