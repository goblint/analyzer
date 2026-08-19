open PostSolver
open QS_update

module ReluctantSimple = QS_reluctant_simple.QSolver
module ReluctantSimpleInlined = QS_inlined_reluctant_simple.QSolver
module ReluctantRestarting = QS_reluctant_restarting
module ReluctantRestartingInlined = QS_inlined_reluctant_restarting
module ReluctantNarrowingStable = QS_reluctant_nstable.QSolver
module ReluctantNarrowingStableInlined = QS_inlined_reluctant_nstable.QSolver

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

let () =
  add ("qrss_w", (module AddPost (ReluctantSimple (WideningUpdate))));
  add ("qrss_wnw", (module AddPost (ReluctantSimple (WNUpdate))));
  add ("qrss_cwnw", (module AddPost (ReluctantSimple (CopyUpdate))));
  add ("qrss_cwnw_wnw", (module AddPost (ReluctantSimple (DefaultUpdate))));
  add ("qrssi_w", (module AddPost (ReluctantSimpleInlined (WideningUpdate))));
  add ("qrssi_wnw", (module AddPost (ReluctantSimpleInlined (WNUpdate))));
  add ("qrssi_cwnw", (module AddPost (ReluctantSimpleInlined (CopyUpdate))));
  add ("qrssi_cwnw_wnw",
       (module AddPost (ReluctantSimpleInlined (DefaultUpdate))))

let () =
  add ("qrsr",
       (module AddPost
            (ReluctantRestarting.Make
               (ReluctantRestarting.Control) (Normal) (CopyUpdate))));
  add ("qrsr_dead",
       (module AddPost
            (ReluctantRestarting.Make
               (ReluctantRestarting.Control) (Dead) (CopyUpdate))));
  add ("qrsr_nosides",
       (module AddPost
            (ReluctantRestarting.Make
               (ReluctantRestarting.Control) (NoSideReset) (CopyUpdate))));
  add ("qrsr_nosides_dead",
       (module AddPost
            (ReluctantRestarting.Make
               (ReluctantRestarting.Control) (NoSideResetDead) (CopyUpdate))));
  add ("qrsri",
       (module AddPost
            (ReluctantRestartingInlined.Make
               (ReluctantRestartingInlined.Control) (Normal) (CopyUpdate))));
  add ("qrsri_dead",
       (module AddPost
            (ReluctantRestartingInlined.Make
               (ReluctantRestartingInlined.Control) (Dead) (CopyUpdate))));
  add ("qrsri_nosides",
       (module AddPost
            (ReluctantRestartingInlined.Make
               (ReluctantRestartingInlined.Control) (NoSideReset) (CopyUpdate))));
  add ("qrsri_nosides_dead",
       (module AddPost
            (ReluctantRestartingInlined.Make
               (ReluctantRestartingInlined.Control)
               (NoSideResetDead) (CopyUpdate))))

let () =
  add ("qrso_w", (module AddPost (ReluctantNarrowingStable (WideningUpdate))));
  add ("qrso_wnw", (module AddPost (ReluctantNarrowingStable (WNUpdate))));
  add ("qrso_cwnw", (module AddPost (ReluctantNarrowingStable (CopyUpdate))));
  add ("qrso_cwnw_wnw",
       (module AddPost (ReluctantNarrowingStable (DefaultUpdate))));
  add ("qrsoi_w",
       (module AddPost (ReluctantNarrowingStableInlined (WideningUpdate))));
  add ("qrsoi_wnw",
       (module AddPost (ReluctantNarrowingStableInlined (WNUpdate))));
  add ("qrsoi_cwnw",
       (module AddPost (ReluctantNarrowingStableInlined (CopyUpdate))));
  add ("qrsoi_cwnw_wnw",
       (module AddPost (ReluctantNarrowingStableInlined (DefaultUpdate))))

(** A value reference from Goblint's existing registration module forces this
    additive module to be linked even when OCaml transparent aliases are used. *)
let registered = ()
