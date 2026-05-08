I will wire it with claude so i can do a smoketest on if the modelling works with a regression test.
functions that are generated and probably must be changed:

For just the smoke-test wiring I added, review these:

SubPoly:

dim_add
dim_remove
meet
leq
join
widen
narrow
unify
VarManagement:

size
ExpressionBounds:

bound_texpr
D:

mpqf_of_z
var_key
map_d
const_mpqf_of_exp
zero_linexpr
add_linexpr
sub_linexpr
option_map2
linexpr_of_exp
interval_of_constraint
simple_constraint
slack_var_of_constraint
row_of_slack
add_constant_interval
add_slack_constraint
printXml
top
is_top
is_bot
meet
leq
join
widen
narrow
unify
forget_var
forget_vars
assign_exp (!)
assign_var
assign_var_parallel
assign_var_parallel_with
assign_var_parallel'
substitute_exp
assert_constraint (!)
invariant
These are the smoke-test placeholders/stubs, not the earlier interval/type work.
