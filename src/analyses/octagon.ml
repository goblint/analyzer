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

  let is_local lval =
    not (lval.vglob ||
         lval.vdecl.line = -1 || (* TODO: Why? *)
         lval.vdecl.line = -3 ||
         lval.vdecl.line = -4)


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
        | None, _ | _, None -> None
        | Some(cl, varl), Some(cr, varr) ->
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
    let lhost, _ = lval in
    let oct, changed =
      (match lhost with
       | Var lval when not (is_local lval) ->
         ctx.local, false
       | Var lval ->
         let rval = stripCastsDeep rval in
         (match rval with
          | BinOp(op, Lval(Var(var), _), Const(CInt64 (integer, _, _)), _) (* TODO: offsets etc? What if the arguments are reversed? *)
            when op = PlusA || op = MinusA ->
            begin
              let integer =
                if op = MinusA
                then Int64.neg integer
                else integer
              in
              if (BV.compare lval var) = 0
              then D.adjust var integer ctx.local, true
              else
                let oct = D.erase lval ctx.local in                     (* integer <= varFromRight-lval <= integer *)
                D.set_constraint (lval, Some(false, var), true, integer)
                  (D.set_constraint (lval, Some(false, var), false, integer) oct), true
            end
          | Lval(Var var, NoOffset) ->
            begin
              if (BV.compare lval var) = 0
              then D.adjust var Int64.zero ctx.local, true
              else
                let oct = D.erase lval ctx.local in                     (* 0 <= varFromRight-lval <= 0 *)
                D.set_constraint (lval, Some(false, var), true, Int64.zero)
                  (D.set_constraint (lval, Some(false, var), false, Int64.zero) oct), true
            end
          | exp ->  (* TODO: What about assigning the value of one variable to the other? *)
            let const = evaluate_exp ctx.local exp in
            let oct = D.erase lval ctx.local in
            if not (INV.is_top const) then
              D.set_constraint (lval, None, true, INV.maximal const |> Option.get)
                (D.set_constraint (lval, None, false, INV.minimal const |> Option.get)
                   oct), true
            else ctx.local, false
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

      let oct, changed =
        (match exp with
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
             | BinOp(op, Lval(Var v1, _), Lval(Var v2, _), _)
               when is_local v1 && is_local v2 && (op = PlusA || op = MinusA) ->
               let sign = (op = PlusA) in
               let oct = if setUpper
                 then D.set_constraint (v1, Some(sign, v2), true, invUpper) ctx.local
                 else ctx.local in
               let oct = if setLower
                 then D.set_constraint (v1, Some(sign, v2), false, invLower) oct
                 else oct in
               oct
             | Lval(Var v, _) when is_local v ->
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
        print_oct oct |> print_endline;
        oct end
      else
        oct
    end

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.top ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()

  let query ctx q =
    let getSumAndDiffForVars exp1 exp2 =
      match exp1, exp2 with
      | Lval(Var v1,NoOffset), Lval(Var v2, NoOffset) -> 
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
            match OctagonDomain.INV.maximal x with
            | Some i when Int64.compare i Int64.zero >= 0 ->
              `Bool(false)
            | _ -> Queries.Result.top ()
          end
        | _ -> Queries.Result.top ()
      end
    | Queries.ExpEq (exp1, exp2) ->                           (* TODO: We want to leverage all the additional information we have here *)
      let inv1, inv2 = evaluate_exp ctx.local exp1,
                       evaluate_exp ctx.local exp2 in
      if INV.is_int inv1 then
        `Bool (INV.compare inv1 inv2 = 0)
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
