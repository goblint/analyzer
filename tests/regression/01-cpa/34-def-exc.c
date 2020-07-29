// --enable ana.int.def_exc --disable ana.int.interval
void main()
{
  char yy[256];
  char *ryy_j = &yy[1];

  int i =0;

  ryy_j++;
  ryy_j++;

  while(i < 2) { // Here
    ryy_j = ryy_j + 1;
    i++;
  }

  return;
}
