// TODO TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main() {
  int i = 1;

  do {
    i++;
    printf("Inside the do-while loop\n");
    if (i % 2 == 0) {

      printf("Skipping %i is even\n", i);
      continue; // This is handled as an goto to line 8 and therefore an up-jumping goto
    }
  } while (i <= 5);

  printf("Exited the loop\n");
  return 0;
}

/*
NOTE:
Test 28: does not terminate but should terminate (test case
"28-do-while-continue-terminating.c") Reason: upjumping goto

If one has a look at the generated CIL output (attached at the bottom of this
file), one can see that the "continue" is translated in a "goto" with a
corresponding label "__Cont". This label points to the loop-exit condition.
Since the condition is part of the loop, its location is evaluated to 8-17. The
location of the goto "goto __Cont" is located in line 15. To provide soundness
for the analysis, the preprocessing detects upjumping gotos with the help of its
location. In case such a goto is detected, the program is classified as
non-terminating. Due to this inserted goto (which is a result of the
"continue"), an upjumping goto is located, which makes this program
non-terminating.

It should be noted that this issue happens when "do while"-loops and "continues"
are combined. If one combines "while"-loops and "continues", the analysis can
still classify the loop as terminating. The reason for that can be seen in the
second CIL output, where the "do while"-loop is replaced by a "while"-loop.
Instead of creating a new label, the "while-continue" label of the loop is
reused. Also, this goto statement is not specified as a goto, but as a Continue
statement. Hence, it is not analyzed for the upjumping gotos, which does not
lead to the problem as with the "do while".


------- SHORTENED CIL output for Test 28 (DO WHILE): -------
int main(void)
{{{{
  #line 8
  while (1) {
    while_continue: ;
    #line 12
    if (i % 2 == 0) {
      #line 15
      goto __Cont;
    }
    __Cont:
    #line 8
    if (! (i <= 5)) {
      #line 8
      goto while_break;
    }
  }

  while_break:
  }}
  #line 20
  return (0);
}}


------- SHORTENED CIL output for Test 28 (WHILE): -------
Test 28: replacing DO WHILE with WHILE: int main(void)
{{{{
  #line 8
  while (1) {
    while_continue: ;
    #line 8
    if (! (i <= 5)) {
      #line 8
      goto while_break;
    }
    #line 12
    if (i % 2 == 0) {
      #line 15
      goto while_continue;
    }
  }
  while_break: ;
  }}
  #line 20
  return (0);
}}

*/
