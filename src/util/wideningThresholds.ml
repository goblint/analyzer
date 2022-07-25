open Cil
module Thresholds = Set.Make(Z)


class extractConstantsVisitor(widening_thresholds,widening_thresholds_incl_mul2) = object
  inherit nopCilVisitor

  method! vexpr e =
    match e with
    | Const (CInt(i,ik,_)) ->
      widening_thresholds := Thresholds.add i !widening_thresholds;
      widening_thresholds_incl_mul2 := Thresholds.add i !widening_thresholds_incl_mul2;
      (* Adding double value of all constants so that we can establish for single variables that they are <= const *)
      (* This is e.g. needed for Apron. Done here where we still have the set representation to avoid expensive    *)
      (* deduplication and sorting on a list later *)
      widening_thresholds_incl_mul2 := Thresholds.add (Z.mul (Z.of_int 2) i) !widening_thresholds_incl_mul2;
      DoChildren
    | _ -> DoChildren
end

let widening_thresholds = ResettableLazy.from_fun (fun () ->
  let set = ref Thresholds.empty in
  let set_incl_mul2 = ref Thresholds.empty in
  let thisVisitor = new extractConstantsVisitor(set,set_incl_mul2) in
  visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
  Thresholds.elements !set, Thresholds.elements !set_incl_mul2)

let thresholds () =
  fst @@ ResettableLazy.force widening_thresholds

let thresholds_incl_mul2 () =
  snd @@ ResettableLazy.force widening_thresholds

module EH = BatHashtbl.Make (CilType.Exp)

class extractInvariantsVisitor (exps) = object
  inherit nopCilVisitor

  method! vinst (i: instr) =
    match i with
    | Call (_, Lval (Var f, NoOffset), args, _, _) ->
      (* TODO: dependency cycle with LibraryFunctions somehow... *)
      (* begin match LibraryFunctions.classify f.vname args with
           | `Assert e ->
             EH.replace exps e ();
             DoChildren
           | _ ->
             DoChildren
         end *)
      begin match f.vname, args with
        | "__goblint_assert", [e] ->
          EH.replace exps e ();
          DoChildren
        | _, _ ->
          DoChildren
      end
    | _ ->
      DoChildren

  method! vstmt (s: stmt) =
    match s.skind with
    | If (e, _, _, _, _)
    | Switch (e, _, _, _, _) ->
      EH.replace exps e ();
      DoChildren
    | _ ->
      DoChildren
end

let exps = ResettableLazy.from_fun (fun () ->
    let exps = EH.create 100 in
    let visitor = new extractInvariantsVisitor exps in
    visitCilFileSameGlobals visitor !Cilfacade.current_file;
    EH.keys exps |> BatList.of_enum
  )

let reset_lazy () =
  ResettableLazy.reset widening_thresholds;
  ResettableLazy.reset exps
