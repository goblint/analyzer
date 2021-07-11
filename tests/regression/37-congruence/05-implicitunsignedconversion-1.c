// PARAM: --enable ana.int.congruence --enable ana.int.congruence_no_overflow
// from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/implicitunsignedconversion-1.c
int main() {
  unsigned int plus_one = 1;
  int minus_one = -1;
  int v = 0;

  if(plus_one < minus_one) {
    v = 1;
    assert(1);
  }
  
  assert(v==1);
  
  // from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/integerpromotion-3.c
  unsigned char port = 0x5a;
  unsigned char result_8 = ( ~port ) >> 4;
  if (result_8 == 0xfa) {
    v = 2;
  }
  
  assert(v==2); // UNKNOWN

  // from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/signextension-1.c
  unsigned short int allbits = -1;
  short int signedallbits = allbits;
  int unsignedtosigned = allbits;
  unsigned int unsignedtounsigned = allbits;
  int signedtosigned = signedallbits;
  unsigned int signedtounsigned = signedallbits;

  /*
  printf ("unsignedtosigned: %d\n", unsignedtosigned);
  printf ("unsignedtounsigned: %u\n", unsignedtounsigned);
  printf ("signedtosigned: %d\n", signedtosigned);
  printf ("signedtounsigned: %u\n", signedtounsigned);
  */

  if (unsignedtosigned == 65535 && unsignedtounsigned == 65535
      && signedtosigned == -1 && signedtounsigned == 4294967295) {
    v =3;
  }

  assert(v==3); // UNKNOWN


  return (0);
}