open Analyses
open Prelude.Ana
open OctagonMapDomain
module INV = IntDomain.Interval32
module BV = Basetype.Variables

let stripCastsDeep e =
  let v = object
    inherit nopCilVisitor
    method vexpr e = ChangeTo (stripCasts e)
  end
  in visitCilExpr v e

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec
  let name = "octagon"
  module D = MapOctagonBot
  module C = D
  module G = Lattice.Unit

  let print_oct oct =
    Prelude.Ana.sprint D.pretty oct

  let is_local_and_not_pointed_to v =
    (not (v.vglob ||
         v.vdecl.line = -1 || (* TODO: Why?  CIL says:The line number. -1 means "do not know"	*)
         v.vdecl.line = -3 ||
         v.vdecl.line = -4))
    && (not v.vaddrof)  (* to avoid handling pointers, only vars whose address is never taken (i.e. can not be pointed to) *)
    && (Cil.isIntegralType v.vtype)


  let evaluate_sums oct exp =
    let match_exp = function
      | BinOp(Mult, Lval(Var(var), _), Const(CInt64 (c, _, _)), _)
      | BinOp(Mult, Const(CInt64 (c, _, _)), Lval(Var(var), _), _) ->
        Some (c, var)
      | Lval(Var(var), _) ->
        Some (Int64.one, var)
      | _ -> None
    in

    match exp with
    | BinOp(op, expl, expr, _) when op = PlusA || op = MinusA ->
      begin match match_exp expl, match_exp expr with
        | Some(cl, varl), Some(cr, varr) when (BV.compare varl varr <> 0) -> (* this is needed as projection with varl=varr throws an exception (?) *)
          let cr = if op = PlusA then cr else Int64.neg cr in
          let cl, cr = INV.of_int cl, INV.of_int cr in
          let varSum = D.projection varl (Some (true, varr)) oct in
          let varDif1 = D.projection varl (Some (false, varr)) oct in
          let varDif2 = D.projection varr (Some (false, varl)) oct in
          let varl = D.projection varl None oct in
          let varr = D.projection varr None oct in
          let candidates = [
            INV.add (INV.mul cl varl) (INV.mul cr varr);
            INV.add (INV.mul cl varSum) (INV.mul (INV.sub cr cl) varr);
            INV.add (INV.mul cr varSum) (INV.mul (INV.sub cl cr) varl);
            INV.add (INV.mul cl varDif1) (INV.mul (INV.add cl cr) varr);
            INV.add (INV.mul cr varDif2) (INV.mul (INV.add cl cr) varl);
          ] in
          Some (List.fold_left INV.meet (INV.top ()) candidates)
        | _ -> None
      end
    | _ -> None

  let rec evaluate_exp oct exp =
    match evaluate_sums oct exp with
    | Some inv -> inv
    | None ->
      begin
        match exp with
        | Const (CInt64 (i, _, _)) -> INV.of_int i
        | Lval (Var var, _) -> D.projection var None oct
        | UnOp (Neg, exp, _) ->
          INV.neg (evaluate_exp oct exp)
        | BinOp (op, expl, expr, _) ->
          let op = (match op with
              | PlusA -> INV.add
              | MinusA -> INV.sub
              | Mult -> INV.mul
              | Div -> INV.div
              | Lt -> INV.lt
              | Gt -> INV.gt
              | Le -> INV.le
              | Ge -> INV.ge
              | Eq -> INV.eq
              | Ne -> INV.ne
              | _ -> fun _ _ -> INV.top ())
          in
          let result = op (evaluate_exp oct expl) (evaluate_exp oct expr) in
          if INV.is_bot result
          then INV.top ()
          else result
        | _ -> INV.top ()
      end


  let assign ctx (lval:lval) (rval:exp) : D.t =
    let oct, changed =
      (match lval with
       | (Var lval, NoOffset) when not (is_local_and_not_pointed_to lval) ->
         ctx.local, false
       | (Var lval, NoOffset) ->                    (* TODO: Are we handling all interesting cases here? *)
         let rval = stripCastsDeep rval in
         let assignVarPlusInt v i =
            if (BV.compare lval v) = 0 then 
              D.adjust v i ctx.local, true
            else
              let oct = D.erase lval ctx.local in                     (* integer <= varFromRight-lval <= integer *)
              D.set_constraint (lval, Some(false, v), true, i)
                (D.set_constraint (lval, Some(false, v), false, i) oct), true
         in
         (match rval with
          | BinOp(op, Lval(Var(var), NoOffset), Const(CInt64 (integer, _, _)), _) 
            when (op = PlusA || op = MinusA) && is_local_and_not_pointed_to var ->
            begin
              let integer = 
                if op = MinusA
                then Int64.neg integer
                else integer
              in
              assignVarPlusInt var integer
            end
          | BinOp(PlusA, Const(CInt64 (integer, _, _)), Lval(Var(var), NoOffset), _) when is_local_and_not_pointed_to var ->
            assignVarPlusInt var integer
          | Lval(Var var, NoOffset) when is_local_and_not_pointed_to var ->
            assignVarPlusInt var Int64.zero
          | exp ->
            let const = evaluate_exp ctx.local exp in
            let oct = D.erase lval ctx.local in
            if not (INV.is_top const) then
              D.set_constraint (lval, None, true, INV.maximal const |> Option.get)
                (D.set_constraint (lval, None, false, INV.minimal const |> Option.get)
                   oct), true
            else oct, false
         )
       | _ -> ctx.local, false)
    in
    if changed
    then begin
      D.strong_closure oct
    end
    else
      oct

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let exp = stripCastsDeep exp in
    let eval = (evaluate_exp ctx.local exp) in
    let eval = if INV.is_bot eval then INV.top () else eval in
    let skip =
      if INV.is_bool eval
      then begin
        let eval_bool = INV.to_bool eval |> BatOption.get in
        eval_bool <> tv
      end
      else false
    in
    if skip
    then
      begin
        raise Deadcode
      end
    else begin
      let negate cmp =
        match cmp with
        | Le -> Gt
        | Lt -> Ge
        | Ge -> Lt
        | Gt -> Le
        | Eq -> Ne
        | Ne -> Eq
        | _ -> cmp
      in
      let equivExpr =
        match exp with
        | BinOp(op, Lval(Var v1, NoOffset), Lval(Var v2, NoOffset), t) when op = Eq || op = Le || op = Lt || op = Ge || op = Gt ->
            BinOp(op, BinOp(MinusA, Lval(Var v1, NoOffset), Lval(Var v2, NoOffset),t), Cil.integer 0, t)
        | _ -> exp
      in
      let oct, changed =                              
        (match equivExpr with
         | BinOp(cmp, lexp, rexp, _) ->
           let cmp = if tv then cmp else negate cmp in
           let inv = evaluate_exp ctx.local rexp in
           let invUpper = INV.maximal inv |> OPT.get in
           let invLower = INV.minimal inv |> OPT.get in
           let setLower, setUpper=
             match cmp with
             | Eq -> true, true
             | Gt | Ge -> true, false
             | Lt | Le -> false, true
             | _ -> false, false
           in
           let setUpper = setUpper && not (Int64.equal invUpper max_int) in
           let setLower = setLower && not (Int64.equal invLower min_int) in
           let invUpper = if cmp = Lt then Int64.sub invUpper Int64.one else invUpper in
           let invLower = if cmp = Gt then Int64.add invLower Int64.one else invLower in
           let oct = begin
             match lexp with
             | BinOp(op, Lval(Var v1, NoOffset), Lval(Var v2, NoOffset), _)
               when is_local_and_not_pointed_to v1 && is_local_and_not_pointed_to v2 && (op = PlusA || op = MinusA) ->
               let sign = (op = PlusA) in
               let oct = if setUpper
                 then D.set_constraint (v1, Some(sign, v2), true, invUpper) ctx.local
                 else ctx.local in
               let oct = if setLower
                 then D.set_constraint (v1, Some(sign, v2), false, invLower) oct
                 else oct in
               oct
             | Lval(Var v, NoOffset) when is_local_and_not_pointed_to v ->
               let oct = if setUpper
                 then D.set_constraint (v, None, true, invUpper) ctx.local
                 else ctx.local in
               let oct = if setLower
                 then D.set_constraint (v, None, false, invLower) oct
                 else oct in
               oct
             | _ -> ctx.local
           end in
           oct, setUpper || setLower
         | _ -> ctx.local, false)
      in

      if changed then begin
        let oct = D.strong_closure (D.meet ctx.local oct) in
        (* print_oct oct |> print_endline; *)
        oct end
      else
        oct
    end

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* remove locals from state *)
    let locals = (f.sformals @ f.slocals) in
    List.fold_left (fun oct v -> D.erase v oct) ctx.local locals

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    match lval with
    | Some (Var v,_) -> D.erase v au (* TODO: Be smarter here *)
    | Some (Mem _, _)
    | None -> au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match lval with
    | Some (Var v,_) -> D.erase v ctx.local
    | Some (Mem _, _)
    | None -> ctx.local
  let startstate v = D.top ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()

  let query ctx q =
    let rec getSumAndDiffForVars exp1 exp2 =
      let addConstant x c = BatOption.map (OctagonDomain.INV.add (OctagonDomain.INV.of_int c)) x in
      match exp1, exp2 with
      | BinOp(PlusA, Lval l1, Const(CInt64(c,_,_)), _), Lval l2 ->
        let sum, diff = getSumAndDiffForVars (Lval l1) (Lval l2) in   (* reason why this is correct a <= x-y <= b -->  *)
        addConstant sum c, addConstant diff c                         (* a+c <= (x+c)-y <= b+c (add c to all sides)    *)
      | Lval l1, BinOp(PlusA, Lval l2, Const(CInt64(c,_,_)), _) ->
        let sum, diff = getSumAndDiffForVars (Lval l1) (Lval l2) in   (* reason why this is correct a <= x-y <= b -->  *)
        addConstant sum (Int64.neg c), addConstant diff (Int64.neg c) (* x-(y+c)= x-y-c --> a-c <= x-(y+c) <= b-c      *)         
      | BinOp(MinusA, Lval l1, Const(CInt64(c,_,_)), _), Lval l2 ->
        let sum, diff = getSumAndDiffForVars (Lval l1) (Lval l2) in   (* reason why this is correct a <= x-y <= b -->  *)
        addConstant sum (Int64.neg c), addConstant diff (Int64.neg c) (* (x-c)-y = x-y-c --> a-c <= (x-c)-y <= b-c     *)
      | Lval l1, BinOp(MinusA, Lval l2, Const(CInt64(c,_,_)), _) ->
        let sum, diff = getSumAndDiffForVars (Lval l1) (Lval l2) in   (* reason why this is correct a <= x-y <= b -->  *)
        addConstant sum c, addConstant diff c                         (* x-(y-c) = x-y+c --> a+c <= x-(y-c) <= b+c     *)
      | Lval(Var v1, NoOffset), Lval(Var v2, NoOffset) -> 
        let sum, diff, flag = D.get_relation v1 v2 ctx.local in
        if not flag then
          sum, diff
        else
          sum, BatOption.map (OctagonDomain.INV.mul (OctagonDomain.INV.of_int Int64.minus_one)) diff
      | _ -> None, None
    in
    match q with
    | Queries.MustBeEqual (exp1,exp2) ->
      begin
        match getSumAndDiffForVars exp1 exp2 with
        | _, Some(x) -> 
          begin
            match OctagonDomain.INV.to_int x with
            | (Some i) -> `Bool (Int64.equal Int64.zero i)
            | _ -> Queries.Result.top ()
          end
        | _ -> Queries.Result.top ()
      end
    | Queries.MayBeEqual (exp1,exp2) ->
      begin
        match getSumAndDiffForVars exp1 exp2 with
        | _, Some(x) -> 
          begin
            if OctagonDomain.INV.is_bot (OctagonDomain.INV.meet x (OctagonDomain.INV.of_int Int64.zero)) then
              `Bool (false)
            else
              Queries.Result.top ()
          end
        | _ -> Queries.Result.top ()
      end
    | Queries.MayBeLess (exp1, exp2) ->
      (* TODO: Here the order of arguments actually matters, be careful *)
      begin
        match getSumAndDiffForVars exp1 exp2 with
        | _, Some(x) -> 
          begin
            match OctagonDomain.INV.minimal x with
            | Some i when Int64.compare i Int64.zero >= 0 ->
              `Bool(false)
            | _ -> Queries.Result.top ()
          end
        | _ -> Queries.Result.top ()
      end
    | Queries.ExpEq (exp1, exp2) ->                           (* TODO: We want to leverage all the additional information we have here *)
      let inv1, inv2 = evaluate_exp ctx.local exp1,           (* Also, what does ExpEq actually do? Is it must or may equality?        *)
                       evaluate_exp ctx.local exp2 in
      if INV.is_int inv1 then
        if INV.is_bot (INV.meet inv1 inv2) then
          `Bool false
        else if INV.compare inv1 inv2 == 0 then
          `Bool true 
        else
          `Top
      else
        `Top
    | Queries.EvalInt exp ->
      let inv = evaluate_exp ctx.local exp in
      if INV.is_int inv
      then `Int(INV.to_int inv |> Option.get)
      else `Top
    | Queries.InInterval (exp, inv) ->
      let linv = evaluate_exp ctx.local exp in
      `Bool (INV.leq linv inv)
    | _ -> Queries.Result.top () 
end


let _ =
  MCP.register_analysis (module Spec : Spec)
