  $ goblint --set solver td3 --set solvers.td3.weak-deps none -v weak-deps.c 2>&1 | grep 'evals'
  [Info] vars = 40    evals = 33    narrow_reuses = 0

  $ goblint --set solver td3 --set solvers.td3.weak-deps eager -v weak-deps.c 2>&1 | grep 'evals'
  [Info] vars = 40    evals = 36    narrow_reuses = 0

  $ goblint --set solver td3 --set solvers.td3.weak-deps lazy -v weak-deps.c 2>&1 | grep 'evals'
  [Info] vars = 40    evals = 32    narrow_reuses = 0

