// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none
// NOCRASH

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.name
D.top
D.is_top
D.is_bot
D.pretty
D.printXml
D.leq
D.join
VarManagement.dim_add
D.assert_constraint
Slack_managment.simple_constraint
Linexpr_managment.linexpr_of_exp
Linexpr_managment.scale_linexpr
Linexpr_managment.normalize_linexpr
Slack_managment.interval_of_constraint_op
Slack_managment.absorb_linexpr_const_into_interval
Slack_managment.slack_var_of_constraint
Slack_managment.row_of_slack
Slack_managment.add_slack_constraint
SubPolyDomain.empty
SubPolyDomain.add_affeq_row
SubPolyDomain.set_slack
SubPolyDomain.mem_slack
SubPolyDomain.string_of
*/

int main(void) {
    int a;
    int b;

    __goblint_assume(a + b < 2);

    return 0;
}
