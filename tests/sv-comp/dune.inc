
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c for_fun_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target for_fun_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff for_fun_true-unreach-call.expected for_fun_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c for_odd_vesal_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target for_odd_vesal_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff for_odd_vesal_true-unreach-call.expected for_odd_vesal_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c for_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target for_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff for_true-unreach-call.expected for_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c global_init_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target global_init_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff global_init_true-unreach-call.expected global_init_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_det_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_det_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_det_false-unreach-call.expected if_det_false-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_det_incr_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_det_incr_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_det_incr_true-unreach-call.expected if_det_incr_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_det_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_det_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_det_true-unreach-call.expected if_det_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_mod_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_mod_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_mod_false-unreach-call.expected if_mod_false-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_mod_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_mod_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_mod_true-unreach-call.expected if_mod_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_nondet_fun_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_nondet_fun_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_nondet_fun_false-unreach-call.expected if_nondet_fun_false-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_nondet_var_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_nondet_var_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_nondet_var_false-unreach-call.expected if_nondet_var_false-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_trier_exclude_multiple_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_trier_exclude_multiple_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_trier_exclude_multiple_false-unreach-call.expected if_trier_exclude_multiple_false-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_trier_exclude_multiple_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_trier_exclude_multiple_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_trier_exclude_multiple_true-unreach-call.expected if_trier_exclude_multiple_true-unreach-call.output)))
  )
  
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c if_trier_exclude_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target if_trier_exclude_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_trier_exclude_true-unreach-call.expected if_trier_exclude_true-unreach-call.output)))
  )
  