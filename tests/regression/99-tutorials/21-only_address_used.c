// SKIP TERM PARAM: --enable ana.wp_run 
int main()
{
  int x = 0;  //this assignment is technically irrellevant, since x's value is not relevant. 
              //still, x is considered a live variable since it is used in a left hand side of an assignment.
              //that could probably be improved..

  int* p1 = &x;
  int* p2 = p1;
  int* p3 = p2;

  x = 10;

  return *p3;
}

// no warnings. 
