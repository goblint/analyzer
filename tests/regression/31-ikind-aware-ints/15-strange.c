// SKIP
// See https://github.com/goblint/cil/issues/29
//PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.enums
int main (int argc, char* argv[])
{
    // This used to cause an excpetion because of incompatible ikinds
    signed char f2 = 7;
    signed char l_1857 = ((0xFFBD4A17L && f2) | f2);
}
