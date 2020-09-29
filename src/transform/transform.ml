open Prelude
open Cil
open Formatcil
open Str

module type S = sig
  val transform : (Cil.location -> Queries.t -> Queries.Result.t) -> file -> unit (* modifications are done in-place by CIL :( *)
end

let h = Hashtbl.create 13
let register name (module T : S) = Hashtbl.add h name (module T : S)
let run name =
  let module T = (val try Hashtbl.find h name with Not_found -> failwith @@ "Transformation "^name^" does not exist!") in
  if GobConfig.get_bool "dbg.verbose" then print_endline @@ "Starting transformation " ^ name;
  T.transform

module PartialEval = struct
  let loc = ref locUnknown (* when we visit an expression, we need the current location -> store at stmts *)
  class visitor ask = object
    inherit nopCilVisitor
    method! vstmt s =
      loc := get_stmtLoc s.skind;
      (* ignore @@ Pretty.printf "Set loc at stmt %a to %a\n" d_stmt s d_loc !loc; *)
      DoChildren
    method! vexpr e =
      let eval e = match ask !loc (Queries.EvalInt e) with
        | `Int i ->
          let e' = integer @@ i64_to_int i in
          ignore @@ Pretty.printf "Replacing non-constant expression %a with %a at %a\n" d_exp e d_exp e' d_loc !loc;
          e'
        | _ ->
          ignore @@ Pretty.printf "Can't replace expression %a at %a\n" d_exp e d_loc !loc; e
      in
      match e with
      | Const _ -> SkipChildren
      | _ -> ChangeDoChildrenPost (e, eval)
  end
  let transform ask file =
    visitCilFile (new visitor ask) file
end
let _ = register "partial" (module PartialEval)

module EvalAssert = struct
  let loc = ref locUnknown (* when we visit an expression, we need the current location -> store at stmts *)
  let ass = ref (makeVarinfo true "unknown" (TVoid []))
  class visitor ask = object
    inherit nopCilVisitor
    method! vglob g =
      match g with
      | GVarDecl (v, l) ->
        if v.vname = "assert" then ass := v;
        DoChildren
      | _ -> DoChildren
    method! vstmt s =
      let construct_assert v s =
        let r = regexp "&&\\|||\\|=>\\|<=\\| > \\| < " in
        global_substitute r (fun s1 ->
                             match matched_string s with
                             | "||" -> "&"
                             | "&&" -> "|"
                             | "=>" -> "<"
                             | "<=" -> ">"
                             | " < " -> " => "
                             | " > " -> " <= "
                             | _ -> "NOP"
                             ) s |> split (regexp " | ")
      in

      let eval_i il =
        let list_map i =
          match i with
          | Set ((lh, lo), e, l) ->
             let ai =
               match lh with
               | Var v -> begin
                  match ask l (Queries.Assert e) with
                  | `Str s ->
                    List.map (fun ca -> cInstr ("%v:assert (!(" ^ ca ^ "));") l [("assert", Fv !ass); ("value", Fv v)]) (construct_assert v s)
                  | _ -> [] end
               | Mem e -> []
               in
               [i] @ ai
          | Call (lv, e, el, l) ->
            begin
            match lv with
              | Some (lh, lo) ->
                let ai =
                  match lh with
                  | Var v -> begin
                    match ask l (Queries.Assert e) with
                    | `Str s ->
                      List.map (fun ca -> cInstr ("%v:assert (!(" ^ ca ^ "));") l [("assert", Fv !ass); ("value", Fv v)]) (construct_assert v s)
                    | _ -> [] end
                  | Mem e -> []
                in

                [i] @ ai
              | None -> [i] end
          | _ -> [i]
        in
        il |> List.map list_map |> List.concat
      in

      let eval_s s =
        match s.skind with
        | Instr il ->
          s.skind <- Instr (eval_i il);
          s
        | _ -> s
      in
      ChangeDoChildrenPost (s, eval_s)
  end
  let transform ask file =
    visitCilFile (new visitor ask) file
end
let _ = register "assert" (module EvalAssert)
