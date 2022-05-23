open Analyses

module Make
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                        and module GVar = GVarF (Spec.V)
                        and module D = Spec.D
                        and module G = Spec.G)
    (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct

  let write lh gh =
    let entries = LHT.fold (fun (n, _) local acc ->
        let context: Invariant.context = {
            scope=Node.find_fundec n;
            i = -1;
            lval=None;
            offset=Cil.NoOffset;
            deref_invariant=(fun _ _ _ -> Invariant.none) (* TODO: should throw instead? *)
          }
        in
        match Spec.D.invariant context local with
        | Some inv ->
          let inv = InvariantCil.exp_replace_original_name inv in
          let loc = Node.location n in
          let entry = `O [
              ("entry_type", `String "loop_invariant");
              ("metadata", `O [
                  ("format_version", `String "0.1");
                ]);
              ("location", `O [
                  ("file_name", `String loc.file);
                  ("line", `Float (float_of_int loc.line));
                  ("column", `Float (float_of_int (loc.column - 1)));
                  ("function", `String (Node.find_fundec n).svar.vname);
                ]);
              ("loop_invariant", `O [
                  ("string", `String (CilType.Exp.show inv));
                  ("type", `String "assertion");
                  ("format", `String "C");
                ]);
            ]
          in
          entry :: acc
        | None ->
          acc
      ) lh []
    in
    let y = `A entries in
    Format.printf "YAML:\n%a\n" Yaml.pp y
end