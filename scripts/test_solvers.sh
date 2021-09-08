# Use this to test all solvers. Not every solver works for every test!
# Test all solvers on a single test: ./scripts/test_solvers.sh evilcollapse_rc
solvers=$(ag 'add_solver \(\K".*?"' src/solvers -o --nofilename --nobreak | sed 's/"//g' | sort)
echo $solvers
for solver in $solvers; do gobopt="--set solver $solver" ./scripts/update_suite.rb $* || echo "Solver $solver failed"; done
