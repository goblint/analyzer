open Cil
open Batteries
module Thresholds = Set.Make(Z)

(*Collect only constants that are used in comparisons*)
(*TODO limit the amount of constants, because too many lead to too many iterations*)
(*differentiating between upper and lower bounds, because e.g. expr > 10 is definitely true for an interval [11, x] and definitely false for an interval [x, 10]*)
(*apron octagons use thresholds for c in inequalities +/- x +/- y <= c*)
(* x + y <= 10 -> definitely right if c=10 *)
(*                definitely wrong if c=-11 (because -x -y <= -11 <=> x + y >= 11 <=> x + y > 10)*)
(* x <= 10 <=> x + x <= 2*10 -> 2 *10 and -2*10-1 are also interesting *)

let addThreshold t_ref z = t_ref := Thresholds.add z !t_ref;;
let one = Z.of_int 1;;
let neg_one = Z.of_int (-1);;

class extractThresholdsFromConditionsVisitor(upper_thresholds,lower_thresholds, octagon_thresholds) = object
  inherit nopCilVisitor

  method! vexpr = function
    (*Comparisons of type: 10 < expr, expr > 10, expr <= 10, 10 >= expr*)
    | BinOp (Lt, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Gt, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Le, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ge, (Const (CInt(i,_,_))), _, (TInt _)) ->
      addThreshold upper_thresholds @@ i;
      addThreshold lower_thresholds @@ Z.add one i;

      addThreshold octagon_thresholds @@ i;
      addThreshold octagon_thresholds @@ Z.sub neg_one i;
      let doubleI = Z.add i i in
      addThreshold octagon_thresholds @@ doubleI;
      addThreshold octagon_thresholds @@ Z.sub neg_one doubleI;
      DoChildren
    (*Comparisons of type: 10 <= expr, expr >= 10, expr < 10, 10 > expr*)
    | BinOp (Le, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Ge, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Lt, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Gt, (Const (CInt(i,_,_))), _, (TInt _)) ->
      addThreshold upper_thresholds @@ Z.add neg_one i;
      addThreshold lower_thresholds @@ i;

      addThreshold octagon_thresholds @@ Z.add neg_one i;
      addThreshold octagon_thresholds @@ Z.neg i;
      let doubleI = Z.add i i in
      addThreshold octagon_thresholds @@ Z.add neg_one doubleI;
      addThreshold octagon_thresholds @@ Z.neg doubleI;
      DoChildren
    (*Comparisons of type: 10 == expr, expr == 10, expr != 10, 10 != expr*)
    | BinOp (Eq, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Eq, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ne, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ne, (Const (CInt(i,_,_))), _, (TInt _)) ->
      addThreshold upper_thresholds @@ Z.add neg_one i;
      addThreshold lower_thresholds @@ Z.add one i;
      addThreshold upper_thresholds @@ i;
      addThreshold lower_thresholds @@ i;

      addThreshold octagon_thresholds @@ i;
      addThreshold octagon_thresholds @@ Z.neg i;
      addThreshold octagon_thresholds @@ Z.sub i one;
      addThreshold octagon_thresholds @@ Z.sub neg_one i;
      let doubleI = Z.add i i in
      addThreshold octagon_thresholds @@ doubleI;
      addThreshold octagon_thresholds @@ Z.neg doubleI;
      addThreshold octagon_thresholds @@ Z.sub doubleI one;
      addThreshold octagon_thresholds @@ Z.sub neg_one doubleI;
      DoChildren
    | _ -> DoChildren
end

let default_thresholds = Thresholds.of_list (
    let thresh_pos = List.map ( Int.pow 2) [0;2;4;8;16;32;48] in
    let thresh_neg = List.map (fun x -> -x) thresh_pos in
    List.map Z.of_int (thresh_neg @ thresh_pos @ [0])
  )

let conditional_widening_thresholds = ResettableLazy.from_fun (fun () ->
    let upper = ref default_thresholds in
    let lower = ref default_thresholds in
    let octagon = ref default_thresholds in
    let thisVisitor = new extractThresholdsFromConditionsVisitor(upper,lower,octagon) in
    visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
    Thresholds.elements !upper, List.rev (Thresholds.elements !lower), Thresholds.elements !octagon )

let upper_thresholds () = 
  let (u,_,_) = ResettableLazy.force conditional_widening_thresholds in u;;

let lower_thresholds () = 
  let (_,l,_) = ResettableLazy.force conditional_widening_thresholds in l;;

let octagon_thresholds () = 
  let (_,_,o) = ResettableLazy.force conditional_widening_thresholds in o;;

(*old version. is there anything this has that the new one does not?*)

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
        | "assert", [e] ->
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
  ResettableLazy.reset conditional_widening_thresholds;
  ResettableLazy.reset exps
