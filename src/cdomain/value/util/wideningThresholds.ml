open GoblintCil
open Batteries
module Thresholds = Set.Make(Z)

(* Collect only constants that are used in comparisons *)
(* For widening we want to have the tightest representation that includes the exit condition *)
(* differentiating between upper and lower bounds, because e.g. expr > 10 is definitely true for an interval [11, x] and definitely false for an interval [x, 10] *)
(* apron octagons use thresholds for c in inequalities +/- x +/- y <= c *)
let addThreshold t_ref z = t_ref := Thresholds.add z !t_ref

class extractThresholdsFromConditionsVisitor(upper_thresholds,lower_thresholds, octagon_thresholds) = object
  inherit nopCilVisitor

  method! vexpr = function
    (* Comparisons of type: 10 <= expr, expr >= 10, expr < 10, 10 > expr *)
    | BinOp (Le, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Ge, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Lt, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Gt, (Const (CInt(i,_,_))), _, (TInt _)) ->
      addThreshold upper_thresholds @@ i;
      addThreshold lower_thresholds @@ Z.pred i;

      let negI = Z.succ @@ Z.neg i in
      addThreshold octagon_thresholds @@ i; (* upper, just large enough: x + Y <= i *)
      addThreshold octagon_thresholds @@ negI; (* lower, just small enough: -X -Y  <= -i+1 -> X + Y >= i-1 -> X + Y >= i-1 *)
      addThreshold octagon_thresholds @@ Z.add i i; (* double upper: X + X <= 2i -> X <= i *)
      addThreshold octagon_thresholds @@ Z.add negI negI; (* double lower: -X -X <= -2i -> X >= i *)
      DoChildren

    (* Comparisons of type: 10 < expr, expr > 10, expr <= 10, 10 >= expr *)
    | BinOp (Lt, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Gt, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Le, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ge, (Const (CInt(i,_,_))), _, (TInt _)) ->
      let i = Z.succ i in (* The same as above with i+1 because for integers expr <= 10 <=> expr < 11 *)
      addThreshold upper_thresholds @@ i;
      addThreshold lower_thresholds @@ Z.pred i;

      let negI = Z.succ @@ Z.neg i in
      addThreshold octagon_thresholds @@ i;
      addThreshold octagon_thresholds @@ negI;
      addThreshold octagon_thresholds @@ Z.add i i;
      addThreshold octagon_thresholds @@ Z.add negI negI;
      DoChildren

    (* Comparisons of type: 10 == expr, expr == 10, expr != 10, 10 != expr *)
    | BinOp (Eq, (Const (CInt(i,_,_))), _, (TInt _))
    | BinOp (Eq, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ne, _, (Const (CInt(i,_,_))), (TInt _))
    | BinOp (Ne, (Const (CInt(i,_,_))), _, (TInt _)) ->
      addThreshold upper_thresholds @@ i;
      addThreshold lower_thresholds @@ i;

      addThreshold octagon_thresholds @@ i;
      addThreshold octagon_thresholds @@ Z.neg i;
      let doubleI = Z.add i i in
      addThreshold octagon_thresholds @@ doubleI;
      addThreshold octagon_thresholds @@ Z.neg doubleI;
      DoChildren
    | _ -> DoChildren
end

let default_thresholds = Thresholds.of_list (
    let thresh_pos = List.map (Int.pow 2) [0;2;4;8;16;32;48] in
    let thresh_neg = List.map (fun x -> -x) thresh_pos in
    List.map Z.of_int (thresh_neg @ thresh_pos @ [0])
  )

let conditional_widening_thresholds = ResettableLazy.from_fun (fun () ->
    let upper = ref default_thresholds in
    let lower = ref default_thresholds in
    let octagon = ref default_thresholds in
    let thisVisitor = new extractThresholdsFromConditionsVisitor(upper,lower,octagon) in
    visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
    !upper, !lower, !octagon)

let upper_thresholds = ResettableLazy.map Tuple3.first conditional_widening_thresholds

let lower_thresholds = ResettableLazy.map Tuple3.second conditional_widening_thresholds

let octagon_thresholds = ResettableLazy.map Tuple3.third conditional_widening_thresholds


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
    !set, !set_incl_mul2)

let thresholds = ResettableLazy.map fst widening_thresholds

let thresholds_incl_mul2 = ResettableLazy.map snd widening_thresholds

module EH = BatHashtbl.Make (CilType.Exp)

class extractInvariantsVisitor (exps) = object
  inherit nopCilVisitor

  method! vinst (i: instr) =
    match i with
    | Call (_, Lval (Var f, NoOffset), args, _, _) when LibraryFunctions.is_special f ->
      Goblint_backtrace.wrap_val ~mark:(Cilfacade.FunVarinfo f) @@ fun () ->
      let desc = LibraryFunctions.find f in
      begin match desc.special args with
        | Assert { exp; _ } ->
          EH.replace exps exp ();
          DoChildren
        | _ ->
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
  ResettableLazy.reset exps;
  ResettableLazy.reset thresholds;
  ResettableLazy.reset thresholds_incl_mul2;
  ResettableLazy.reset upper_thresholds;
  ResettableLazy.reset lower_thresholds;
  ResettableLazy.reset octagon_thresholds;
