// Variant of nested.c with a counter.
void main()
{
  int z = 0;
  for (int i=0; i<10 ; i++) {
	  z = z+1;
	  for (int j = 0; j < 10 ; j++) ;
    z = z+1; // was this intended to be inner loop?
  }
  return ;
}
