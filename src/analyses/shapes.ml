open Cil
open Pretty
open Analyses

open ShapeDomain

module GU = Goblintutil
module Re = Region.Spec

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Shape Analysis for Cyclic Doubly Linked Lists"
  module Dom  = ShapeDomain.Dom
  module Glob = Global.Make (IntDomain.Booleans)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  let rec tryReallyHard ask gl upd f st =
    try f st
    with PleaseMaterialize k ->
      let st = Dom.union 
                  (Dom.map (SHMap.add ask gl upd k (empty_list k   )) st) 
                  (Dom.map (SHMap.add ask gl upd k (nonempty_list k)) st)
      in
      assert (Dom.for_all (fun sm -> SHMap.mem k sm) st);
      tryReallyHard ask gl upd f st
      
  let vars e = 
    let module S = Set.Make (Var) in
    let rec offs_contains o =
      match o with
        | Cil.NoOffset -> S.empty
        | Cil.Field (_,o) -> offs_contains o
        | Cil.Index (e,o) -> S.union (cv e) (offs_contains o)
    and cv e = 
      match e with
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.Const _ 
        | Cil.AlignOfE _ -> S.empty
        | Cil.UnOp  (_,e,_)     -> cv e      
        | Cil.BinOp (_,e1,e2,_) -> S.union (cv e1) (cv e2)  
        | Cil.AddrOf  (Cil.Mem e,o) 
        | Cil.StartOf (Cil.Mem e,o) 
        | Cil.Lval    (Cil.Mem e,o) -> S.union (cv e) (offs_contains o)
        | Cil.CastE (_,e)           -> cv e 
        | Cil.Lval    (Cil.Var v2,o) -> S.add v2 (offs_contains o)
        | Cil.AddrOf  (Cil.Var v2,o) 
        | Cil.StartOf (Cil.Var v2,o) -> S.add v2 (offs_contains o)
    in
    S.elements (cv e)

  let sync ctx =
    let f sm (st, ds)=
      let (nsm,nds) = sync_one ctx.ask ctx.global ctx.geffect sm in
      (Dom.add nsm st, nds@ds)
    in
    Dom.fold f ctx.local (Dom.empty (), [])

  let sync ctx =
    tryReallyHard ctx.ask ctx.global ctx.geffect (fun st -> sync (swap_st ctx st)) ctx.local

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    let st = ctx.local in
(*     try  *)
    match eval_lp ctx.ask (Lval lval), eval_lp ctx.ask rval with
      | Some (l,`Next), Some (r,`NA) -> Dom.map (normal ctx.ask ctx.global ctx.geffect l `Next r) st
      | Some (l,`Prev), Some (r,`NA) -> Dom.map (normal ctx.ask ctx.global ctx.geffect l `Prev r) st
      | Some (l,`NA), Some (r,dir) -> 
          Dom.fold (fun d xs -> List.fold_right Dom.add (add_alias ctx.ask ctx.global ctx.geffect l (r,dir) d) xs) st (Dom.empty ()) 
      | _ -> 
          let ls = vars (Lval lval) in
          Dom.map (kill_vars ctx.ask ctx.global ctx.geffect ls) ctx.local

(*     with x -> ignore (Pretty.printf "Exception %s \n" (Printexc.to_string x)); st *)
  
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    tryReallyHard ctx.ask ctx.global ctx.geffect (fun st -> assign (swap_st ctx st) lval rval) ctx.local

  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    let xor x y = if x then not y else y in
    let invariant lp1 lp2 b = 
      let inv_one (sm:SHMap.t) (xs:Dom.t) =
        if xor (xor tv b) (must_alias ctx.ask ctx.global lp1 lp2 sm)
        then Dom.add sm xs 
        else xs
      in
      Dom.fold inv_one ctx.local (Dom.empty ())
    in
    let eval_lps e1 e2 b = 
      match eval_lp ctx.ask e1, eval_lp ctx.ask e2 with
        | Some lpe1, Some lpe2 -> invariant lpe1 lpe2 b
        | _ -> ctx.local 
    in
    match stripCasts exp with
      | BinOp (Ne,e1,e2,_) -> eval_lps (stripCasts e1) (stripCasts e2) false 
      | BinOp (Eq,e1,e2,_) -> eval_lps (stripCasts e1) (stripCasts e2) true
      | _ -> ctx.local
  
  let branch ctx exp tv =
    tryReallyHard ctx.ask ctx.global ctx.geffect (fun st -> branch (swap_st ctx st) exp tv) ctx.local

  let body ctx (f:fundec) : Dom.t = 
    MyLiveness.computeLiveness f; 
    ctx.local  
    
  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    Dom.map (kill_vars ctx.ask ctx.global ctx.geffect (f.sformals @ f.slocals)) ctx.local

  let return ctx exp f =
    tryReallyHard ctx.ask ctx.global ctx.geffect (fun st -> return (swap_st ctx st) exp f) ctx.local

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let rec zip xs ys =
      match xs, ys with
        | x::xs, y::ys -> (x, y) :: zip xs ys 
        | _ -> []
    in
    let fd = Cilfacade.getdec f in
    let asg (v,e) d = assign (swap_st ctx d) (Var v,NoOffset) e in
    [ctx.local, List.fold_right asg (zip fd.sformals args) ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let lift_st x = [x, Cil.integer 1, true] in
    match f.vname, arglist with
      | "kill", [ee] -> begin
          match eval_lp ctx.ask ee with
            | Some (lp, _) -> lift_st (Dom.map (kill ctx.ask ctx.global ctx.geffect lp) ctx.local)
            | _ -> lift_st ctx.local
        end
      | "collapse", [e1;e2] -> begin
          match eval_lp ctx.ask e1, eval_lp ctx.ask e2 with
            | Some (lp1, _), Some (lp2, _) -> lift_st (Dom.map (collapse_summary ctx.ask ctx.global ctx.geffect lp1 lp2) ctx.local)
            | _ -> lift_st ctx.local
        end
      | _ -> 
    match lval with
      | None -> lift_st ctx.local
      | Some x -> 
    let ls = vars (Lval x) in
    lift_st (Dom.map (kill_vars ctx.ask ctx.global ctx.geffect ls) ctx.local) 
    
  let startstate () = Dom.singleton (SHMap.top ())
  let otherstate () = Dom.singleton (SHMap.top ())
end

module ShapeMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "shape" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Shape x
                let extract_l x = match x with `Shape x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Shapes x
                let extract_g x = match x with `Shapes x -> x | _ -> raise MCP.SpecificationConversionError
         end)
