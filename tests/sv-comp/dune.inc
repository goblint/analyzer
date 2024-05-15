
  (subdir basic
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c for_fun_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target for_fun_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
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
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff if_trier_exclude_true-unreach-call.expected if_trier_exclude_true-unreach-call.output)))
  )
  
  (subdir eq
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c eq_double_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target eq_double_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff eq_double_true-unreach-call.expected eq_double_true-unreach-call.output)))
  )
  
  (subdir eq
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c eq_single_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target eq_single_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff eq_single_true-unreach-call.expected eq_single_true-unreach-call.output)))
  )
  
  (subdir eq
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multivar_false-unreach-call1.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multivar_false-unreach-call1.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multivar_false-unreach-call1.expected multivar_false-unreach-call1.output)))
  )
  
  (subdir eq
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multivar_true-unreach-call1.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multivar_true-unreach-call1.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multivar_true-unreach-call1.expected multivar_true-unreach-call1.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_context_join_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_context_join_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_context_join_true-unreach-call.expected multicall_context_join_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_context_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_context_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_context_true-unreach-call.expected multicall_context_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_join_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_join_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_join_true-unreach-call.expected multicall_join_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_nested_join_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_nested_join_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_nested_join_true-unreach-call.expected multicall_nested_join_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_nested_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_nested_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_nested_true-unreach-call.expected multicall_nested_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_return_context_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_return_context_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_return_context_true-unreach-call.expected multicall_return_context_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c multicall_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target multicall_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff multicall_true-unreach-call.expected multicall_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c join_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target join_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff join_true-unreach-call.expected join_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c builtin_expect_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target builtin_expect_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff builtin_expect_true-unreach-call.expected builtin_expect_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c region_global_init_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target region_global_init_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff region_global_init_true-unreach-call.expected region_global_init_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c local_shadow_fun_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target local_shadow_fun_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff local_shadow_fun_true-unreach-call.expected local_shadow_fun_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c free_spawn_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target free_spawn_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff free_spawn_true-unreach-call.expected free_spawn_true-unreach-call.output)))
  )
  
  (subdir cfg
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c free_spawn_ub_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target free_spawn_ub_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff free_spawn_ub_true-unreach-call.expected free_spawn_ub_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and3_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and3_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and3_true-unreach-call.expected and3_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and3dead_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and3dead_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and3dead_true-unreach-call.expected and3dead_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and_copy_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and_copy_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and_copy_true-unreach-call.expected and_copy_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and_join_invariant_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and_join_invariant_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and_join_invariant_true-unreach-call.expected and_join_invariant_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and_true-unreach-call.expected and_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and_var_false-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and_var_false-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and_var_false-unreach-call.expected and_var_false-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c and_var_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target and_var_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff and_var_true-unreach-call.expected and_var_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c or3_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target or3_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff or3_true-unreach-call.expected or3_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c or3dead_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target or3dead_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff or3dead_true-unreach-call.expected or3dead_true-unreach-call.output)))
  )
  
  (subdir cfg/uncil
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c or_true-unreach-call.c) (:prop %{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target or_true-unreach-call.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %{prop} %{c} --enable exp.argdot --enable exp.arg --set witness.graphml.id enumerate))
       (with-stdout-to %{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff or_true-unreach-call.expected or_true-unreach-call.output)))
  )
  