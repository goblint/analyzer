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

let widening_thresholds = lazy (
  let set = ref Thresholds.empty in
  let set_incl_mul2 = ref Thresholds.empty in
  let thisVisitor = new extractConstantsVisitor(set,set_incl_mul2) in
  visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
  Thresholds.elements !set, Thresholds.elements !set_incl_mul2)

let thresholds () =
  fst @@ Lazy.force widening_thresholds

let thresholds_incl_mul2 () =
  snd @@ Lazy.force widening_thresholds
