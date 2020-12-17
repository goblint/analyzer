open Prelude
open Cil
open Formatcil
open Str

module EvalAssert = struct
  let loc = ref locUnknown (* when we visit an expression, we need the current location -> store at stmts *)
  let ass = ref (makeVarinfo true "unknown" (TVoid []))
  let locals = ref []
  class visitor ask = object(self)
    inherit nopCilVisitor
    method! vfunc f =
      locals := !locals @ f.slocals;
      DoChildren
    method! vglob g =
      match g with
      | GVarDecl (v, l) ->
        if v.vname = "assert" then begin
          ass := v;
          SkipChildren end
        else DoChildren
      | _ -> DoChildren
    method! vstmt s =
      let construct_assert s =
        let r = regexp "&&\\|||" in
        global_substitute r (fun s1 ->
                             match matched_string s with
                             | "||" -> "|"
                             | "&&" -> "&"
                             | _ -> "NOP"
                             ) s
      in

      let ai lh loc =
        match lh with
        | Var v ->
          let v_e = cExp "%v:variable" [("variable", Fv v)] in
          (match ask loc (Queries.Assert v_e) with
            | `ExprSet s ->
              let e = Queries.ES.choose s in
              [cInstr ("%v:assert (%e:exp);") loc [("assert", Fv !ass); ("value", Fv v); ("exp", Fe e)]]
            | _ -> [])
        | Mem e -> []
      in

      let rec eval_i il s =
        match il with
        | i1 :: i2 :: xs -> begin try
          begin
          match i1 with
          | Set ((lh, _), _, _) -> [i1] @ (ai lh (get_instrLoc i2)) @ eval_i (i2 :: xs) s
          | Call (lv, _, _, _) ->
            begin
              match lv with
                | Some (lh, _) -> [i1] @ (ai lh (get_instrLoc i2)) @ eval_i (i2 :: xs) s
                | None -> []
            end
          | _ -> i1 :: eval_i (i2 :: xs) s
          end with e -> i1 :: eval_i (i2 :: xs) s end
        | [i] ->
          if List.length s.succs > 0 && (List.hd s.succs).preds |> List.length <> 2 then begin
            (* try *)
              let l = get_stmtLoc (List.hd s.succs).skind in
              (match i with
              | Set ((lh, _), _, _) -> [i] @ (ai lh l)
              | Call (lv, _, _, _) ->
                begin
                  match lv with
                    | Some (lh, _) -> [i] @ (ai lh l)
                    | None -> []
                end
              | _ -> [i])
            (* with e -> [] *)
          end
          else [i]
        | _ -> []
      in

      let rec get_vars e =
        match e with
        | Lval (lh, _) -> begin
          match lh with
          | Var v ->
            [v]
          | _ -> [] end
        | UnOp (u, ex, t) -> get_vars ex
        | BinOp (b, e1, e2, t) -> (get_vars e1) @ (get_vars e2)
        | _ -> []
      in

      let eval_s s =
        if List.length s.preds = 2 then begin
          let p1 = List.hd s.preds in
          let p1_loc = get_stmtLoc p1.skind in

          let p2 = s.preds |> List.rev |> List.hd in
          let p2_loc = get_stmtLoc p2.skind in

          let values loc vs =
            List.map
              (fun v -> try
                let v_e = cExp "%v:variable" [("variable", Fv v)] in
                match ask loc (Queries.Assert v_e) with
                | `Str s -> (v, s)
                | _ -> (v, "none") with e -> (v, "none")) vs
          in
          let p1_values = values p1_loc !locals in
          let p2_values = values p2_loc !locals in
          let join_loc = get_stmtLoc s.skind in

          let last_instruction s =
            match s with
            | Instr il ->
              if List.length il > 0 then
                let instr = il |> List.hd in
                begin
                match instr with
                | Set ((lh, lo), e, l) ->
                  let v_instr =
                    match lh with
                    | Var v -> Some (v, string_of_int l.line)
                    | Mem e -> None
                  in v_instr
                | Call (lv, e, el, l) ->
                  begin
                  match lv with
                    | Some (lh, lo) ->
                      let v_instr =
                        match lh with
                        | Var v -> Some (v, string_of_int l.line)
                        | Mem e -> None
                      in v_instr
                    | None -> None end
                | _ -> None
                end
              else None
            | _ -> None
          in

          let last_p1 = last_instruction p1.skind in
          let last_p2 = last_instruction p2.skind in

          let asserts =
            List.map2
              (fun (v1, s1) (v2, s2) -> try
                let new_s1 =
                  match last_p1 with
                  | Some (v, s) when v.vname = v1.vname -> s
                  | _ -> s1
                in
                let new_s2 =
                  match last_p2 with
                  | Some (v, s) when v.vname = v2.vname -> s
                  | _ -> s2
                in
                if new_s1 <> new_s2 then begin
                  let v_e = cExp "%v:variable" [("variable", Fv v1)] in
                  match ask join_loc (Queries.Assert v_e) with
                  | `Str s -> [cInstr ("%v:assert (" ^ (construct_assert s) ^ ");") join_loc [("assert", Fv !ass); ("value", Fv v1)]]
                  | _ -> [] end
                else [] with e -> []) p1_values p2_values |> List.concat
          in
          self#queueInstr asserts;
          end;

        match s.skind with
        | Instr il ->
          s.skind <- Instr (eval_i il s);
          s
        | If (e, b1, b2, l) ->
          let vars = get_vars e in
          let asserts loc vs =
            List.map
              (fun v -> try
                (let v_e = cExp "%v:variable" [("variable", Fv v)] in
                match ask loc (Queries.Assert v_e) with
                | `Str s -> [cInstr ("%v:assert (" ^ (construct_assert s) ^ ");") loc [("assert", Fv !ass); ("value", Fv v)]]
                | _ ->
                  []) with e -> []) vs |> List.concat
          in

          begin
          try
            let b1_ass =
              let b1_loc = get_stmtLoc (List.hd b1.bstmts).skind in
              let i1 = (asserts b1_loc vars) in
              [cStmt "{ %I:asserts %S:b1 }" (fun n t -> makeVarinfo true "unknown" (TVoid []))
                    b1_loc [("asserts", FI i1); ("b1", FS b1.bstmts)]]
            in
            b1.bstmts <- b1_ass;
          with e -> () end;

          begin
          try
            let b2_ass =
              if b2.bstmts = [] then [] else
                let b2_loc = get_stmtLoc (List.hd b2.bstmts).skind in
                let i2 = (asserts b2_loc vars) in
                [cStmt "{ %I:asserts %S:b2 }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b2_loc [("asserts", FI i2); ("b2", FS b2.bstmts)]]
            in
            b2.bstmts <- b2_ass;
          with e -> () end;
          s
        | _ -> s
      in
      ChangeDoChildrenPost (s, eval_s)
  end
  let transform ask file = begin
    visitCilFile (new visitor ask) file;
    let assert_filename = global_replace (regexp "\\(.*\\)\\(/[^/]+\\)\\(/[^/]+\\.c\\)") "\\1/assert_transform\\3" file.fileName in
    let assert_dir = global_replace (regexp "\\(.*\\)\\(/[^/]+\\)\\(/[^/]+\\.c\\)") "\\1/assert_transform" file.fileName in
    let _ =
      try
        Unix.mkdir assert_dir 0o770
      with Unix.Unix_error(err, ctx1, ctx) as ex ->
        if err != Unix.EEXIST then begin
          print_endline ("Error, " ^ (Unix.error_message err));
          raise ex
        end
    in
    let oc = Stdlib.open_out assert_filename in
    dumpFile defaultCilPrinter oc assert_filename file; end
end
let _ = Transform.register "assert" (module EvalAssert)
