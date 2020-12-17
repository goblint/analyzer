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

    method! vglob g = match g with
      | GVarDecl (v, l) ->
        if v.vname = "assert" then begin
          ass := v;
          SkipChildren end
        else DoChildren
      | _ -> DoChildren

    method! vstmt s =
      let make_assert loc var =
        match ask loc (Queries.Assert (Lval(Var var, NoOffset))) with
        | `ExprSet s ->
          let e = Queries.ES.choose s in
          [cInstr ("%v:assert (%e:exp);") loc [("assert", Fv !ass); ("exp", Fe e)]]
        | _ -> []
      in

      let ai lh loc =
        match lh with
        | Var v -> make_assert loc v
        | Mem e -> []
      in

      let rec eval_i il s =
        match il with
        | i1 :: i2 :: xs ->
          begin
          match i1 with
          | Set ((lh, _), _, _) -> [i1] @ (ai lh (get_instrLoc i2)) @ eval_i (i2 :: xs) s
          | Call (Some (lh,_), _, _, _) -> [i1] @ (ai lh (get_instrLoc i2)) @ eval_i (i2 :: xs) s
          | _ -> i1 :: eval_i (i2 :: xs) s
          end
        | [i] ->
          if List.length s.succs > 0 && (List.hd s.succs).preds |> List.length <> 2 then begin
              let l = get_stmtLoc (List.hd s.succs).skind in
              (match i with
              | Set ((lh, _), _, _) -> [i] @ (ai lh l)
              | Call (Some (lh, _), _, _, _) -> [i] @ (ai lh l)
              | _ -> [i])
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
              (fun v ->
                match ask loc (Queries.Assert (Lval (Var v,NoOffset))) with
                | `ExprSet s -> (v, Some(Queries.ES.choose s))
                | _ -> (v, None)) vs
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

          (* what is the goal here? o.O *)
          let asserts =
            List.map2
              (fun (v1, (s1:exp option)) (v2, (s2:exp option)) ->
                let new_s1 =
                  match last_p1 with
                  | Some (v, s) when v.vname = v1.vname -> Error s
                  | _ -> Ok s1
                in
                let new_s2 =
                  match last_p2 with
                  | Some (v, s) when v.vname = v2.vname -> Error s
                  | _ -> Ok s2
                in
                if new_s1 <> new_s2 then (* comparing exp for equality is dangerous *)
                  make_assert join_loc v1
                else []) p1_values p2_values |> List.concat
          in
          self#queueInstr asserts;
          end;

        match s.skind with
        | Instr il ->
          s.skind <- Instr (eval_i il s);
          s
        | If (e, b1, b2, l) ->
          let vars = get_vars e in
          let asserts loc vs = List.map (make_assert loc) vs |> List.concat in
          let add_asserts block =
            if List.length block.bstmts > 0 then
              let with_asserts =
                let b_loc = get_stmtLoc (List.hd block.bstmts).skind in
                let b_assert_instr = asserts b_loc vars in
                [cStmt "{ %I:asserts %S:b }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b_loc [("asserts", FI b_assert_instr); ("b", FS block.bstmts)]]
              in
              block.bstmts <- with_asserts
            else
              ()
          in

          add_asserts b1;
          add_asserts b2;
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
