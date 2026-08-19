open PostSolver
open QS_update

module QSs  = QS_simple.QSolver
module QSsi = QS_inlined_simple.QSolver
module QSg  = QS_restarting
module QSgi = QS_inlined_restarting
module QSo  = QS_nstable.QSolver
module QSoi = QS_inlined_nstable.QSolver

let add = Selector.add_solver

module Normal = struct
  let side_effect_phase_reset = true
  let dead_side_elimination = true
end

module Dead = struct
  let side_effect_phase_reset = true
  let dead_side_elimination = false
end

module NoSideReset = struct
  let side_effect_phase_reset = false
  let dead_side_elimination = true
end

module NoSideResetDead = struct
  let side_effect_phase_reset = false
  let dead_side_elimination = false
end


(* --------- *)


let _ =
  add ("qss_w",    (module AddPost (QSs (WideningUpdate))));
  add ("qss_wnw",  (module AddPost (QSs (WNUpdate))));
  add ("qss_cwnw", (module AddPost (QSs (CopyUpdate))));
  add ("qss_cwnw_w", (module AddPost (QSs (DefaultUpdate))))

let _ =
  add ("qssi_w",    (module AddPost (QSsi (WideningUpdate))));
  add ("qssi_wnw",  (module AddPost (QSsi (WNUpdate))));
  add ("qssi_cwnw", (module AddPost (QSsi (CopyUpdate))));
  add ("qssi_cwnw_w", (module AddPost (QSsi (DefaultUpdate))))


(* --------- *)


let _ =
  add ("qsr",    
       (module AddPost (QSg.Make (QSg.Control) (Normal) (CopyUpdate))));
  add ("qsr_dead",    
       (module AddPost (QSg.Make (QSg.Control) (Dead) (CopyUpdate))));
  add ("qsr_nosides",    
       (module AddPost (QSg.Make (QSg.Control) (NoSideReset) (CopyUpdate))));
  add ("qsr_nosides_dead",    
       (module AddPost (QSg.Make (QSg.Control) (NoSideResetDead) (CopyUpdate))));
  add ("qsr_nosides_dead_w",    
       (module AddPost (QSg.Make (QSg.Control) (NoSideResetDead) (DefaultUpdate))))

(* --------- *)

let _ =
  add ("qsri",    
       (module AddPost (QSgi.Make (QSg.Control) (Normal) (CopyUpdate))));
  add ("qsri_dead",    
       (module AddPost (QSgi.Make (QSg.Control) (Dead) (CopyUpdate))));
  add ("qsri_nosides",    
       (module AddPost (QSgi.Make (QSg.Control) (NoSideReset) (CopyUpdate))));
  add ("qsri_nosides_dead",    
       (module AddPost (QSgi.Make (QSg.Control) (NoSideResetDead) (CopyUpdate))))

(* --------- *)
let _ =
  add ("qso_w",    (module AddPost (QSo (WideningUpdate))));
  add ("qso_wnw",  (module AddPost (QSo (WNUpdate))));
  add ("qso_cwnw", (module AddPost (QSo (CopyUpdate))));
  add ("qso_cwnw_w", (module AddPost (QSo (DefaultUpdate))))


let _ =
  add ("qsoi_w",    (module AddPost (QSoi (WideningUpdate))));
  add ("qsoi_wnw",  (module AddPost (QSoi (WNUpdate))));
  add ("qsoi_cwnw", (module AddPost (QSoi (CopyUpdate))));
  add ("qsoi_cwnw_w", (module AddPost (QSoi (DefaultUpdate))))



(* QSOLVER_RELUCTANT_BEGIN *)
let () = QS_reluctant_register.registered
(* QSOLVER_RELUCTANT_END *)
