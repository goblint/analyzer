// PARAM: --enable ana.int.congruence --enable ana.int.congruence_no_overflow
// from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/implicitunsignedconversion-1.c
int main() {
  unsigned int plus_one = 1;
  int minus_one = -1;
  int v1 = 0;
  int v2 = 0;
  int v3 = 0;
  int v4 = 0;

  if(plus_one < minus_one) {
    v1 = 1;
    assert(1);
  }

  assert(v1==1);

  // from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/integerpromotion-3.c
  unsigned char port = 0x5a;
  unsigned char result_8 = ( ~port ) >> 4;
  if (result_8 == 0xfa) {
    v2 = 2;
  }

  assert(v2==2); // UNKNOWN

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
    v3=3;
  }

  assert(v3==3); // UNKNOWN

  // from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/bitvector-regression/signextension2-2.c
  unsigned int allOne = -1;

  int castToInt = allOne;
  long castToLong = allOne;
  long castToLong2 = castToInt;
  unsigned long castToULong = allOne;

  if (castToInt == -1 && castToLong == 4294967295UL &&
      castToLong2 == -1 && castToULong == 4294967295UL) {
    v4=4;
  }

  assert(v4==4); //UNKNOWN

  return (0);
}
