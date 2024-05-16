let generate_rule file =
  let dir = Filename.dirname file in
  let file'' = Filename.basename file in
  let file' = Filename.chop_extension file'' in
  Printf.printf {|
  (subdir %s
    (rule
     (alias runtest)
     (deps (sandbox always) (package goblint) (:c %s) (:prop %%{project_root}/tests/sv-comp/unreach-call-__VERIFIER_error.prp))
     (target %s.output)
     (action
      (progn
       (ignore-outputs (run goblint --conf svcomp21.json --set ana.specification %%{prop} %%{c} --enable exp.argdot --enable exp.arg --set exp.argdotlabel empty))
       (with-stdout-to %%{target} (run graph-easy --as=boxart arg.dot)))))

    (rule
     (alias runtest)
     (action (diff %s.expected %s.output)))
  )
  |} dir file'' file' file' file'

let () =
  Sys.argv
  |> Array.to_seq
  |> Seq.drop 1
  |> Seq.iter generate_rule
