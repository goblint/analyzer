let generate_rule c_dir_file =
  let dir = Filename.dirname c_dir_file in
  let c_file = Filename.basename c_dir_file in
  let file = Filename.chop_extension c_file in
  Printf.printf {|
(subdir %s
 (rule
  (deps
   (sandbox always) ; must sandbox to prevent arg.dot-s from parallel runs from interfering
   (package goblint) ; depend on entire goblint package for svcomp21 conf
   (:c %s)
   (:prop %%{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
  (target %s.output)
  (enabled_if %%{bin-available:graph-easy})
  (action
   (progn
    (ignore-outputs
     (run goblint --conf svcomp21.json --set ana.specification %%{prop} %%{c} --enable exp.argdot --enable exp.arg --set exp.argdotlabel empty))
    (with-stdout-to %%{target}
     (run graph-easy --as=boxart arg.dot)))))

 (rule
  (alias runtest)
  (enabled_if %%{bin-available:graph-easy})
  (action
   (diff %s.expected %s.output))))
|} dir c_file file file file

let () =
  Sys.argv
  |> Array.to_seq
  |> Seq.drop 1
  |> Seq.iter generate_rule
