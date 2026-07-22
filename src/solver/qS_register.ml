open PostSolver
open QS_update

module QSt = QS_trivial.QSolver
module QSg = QS.QSolver
module QSi = QS_inlined.QSolver

let _ =
  Selector.add_solver ("qs_trivial", (module AddPost  (QSt)))

module QSolverW    = QSg (WideningUpdate)
module QSolverWNW  = QSg (WNUpdate)
module QSolverCWNW = QSg (CopyUpdate)

let _ =
  Selector.add_solver ("qs_w",    (module AddPost  (QSolverW)));
  Selector.add_solver ("qs_wnw",  (module AddPost  (QSolverWNW)));
  Selector.add_solver ("qs_cwnw", (module AddPost  (QSolverCWNW)))

module QSolveriW    = QSi (WideningUpdate)
module QSolveriWNW  = QSi (WNUpdate)
module QSolveriCWNW = QSi (CopyUpdate)

let _ =
  Selector.add_solver ("qsi_w",    (module AddPost  (QSolveriW)));
  Selector.add_solver ("qsi_wnw",  (module AddPost  (QSolveriWNW)));
  Selector.add_solver ("qsi_cwnw", (module AddPost  (QSolveriCWNW)))
