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

module FeaturesDead : QS_inlined.FEATURES = struct
  let normal_phase_reset = true
  let side_effect_phase_reset = true
  let dead_side_elimination = false
end

module FeaturesNoSideReset : QS_inlined.FEATURES = struct
  let normal_phase_reset = true
  let side_effect_phase_reset = false
  let dead_side_elimination = true
end

module FeaturesNoSideResetDead : QS_inlined.FEATURES = struct
  let normal_phase_reset = true
  let side_effect_phase_reset = false
  let dead_side_elimination = false
end

module FeaturesNoReset : QS_inlined.FEATURES = struct
  let normal_phase_reset = false
  let side_effect_phase_reset = false
  let dead_side_elimination = true
end

module FeaturesNoResetDead : QS_inlined.FEATURES = struct
  let normal_phase_reset = false
  let side_effect_phase_reset = false
  let dead_side_elimination = false
end

module QS_inl = QS_inlined.Make (QS_inlined.Control)

let _ =
  Selector.add_solver ("qsi_dead",    
                       (module AddPost  (QS_inl (FeaturesDead) (CopyUpdate))));
  Selector.add_solver ("qsi_reset1",    
                       (module AddPost  (QS_inl (FeaturesNoSideReset) (CopyUpdate))));
  Selector.add_solver ("qsi_reset1_dead",    
                       (module AddPost  (QS_inl (FeaturesNoSideResetDead) (CopyUpdate))));
  Selector.add_solver ("qsi_reset2",    
                       (module AddPost  (QS_inl (FeaturesNoReset) (CopyUpdate))));
  Selector.add_solver ("qsi_reset2_dead",    
                       (module AddPost  (QS_inl (FeaturesNoResetDead) (CopyUpdate))));

