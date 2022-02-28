
open Prelude.Ana
open Analyses

(* For different kinds of value domains, like addresses, indexes... *)
include PreValueDomain

(* Program point lattice *)
module PL = Lattice.Flat (Node) (struct
    let top_name = "Unknown node"
    let bot_name = "Unreachable node"
  end)

(* Variable lattice *)
module VL = Lattice.Flat (Basetype.Variables) (struct
    let top_name = "Unknown variable"
    let bot_name = "Unreachable variable"
  end)

(* Origin is Variable and Node *)
(* TODO: instead of Node, use Unknown to be able to split only in certain contexts *)
module Origin = Lattice.Prod (VL) (PL)
module OriginSet = SetDomain.ToppedSet (Origin) (struct let topname = "All" end)
module ValueOriginPair = Lattice.Prod (AD) (OriginSet)

(* Map of Variable -> Pair (AddressSet, OriginSet) *)
module OriginMap = struct
  include MapDomain.MapBot (Basetype.Variables) (ValueOriginPair)
  type t = MapDomain.MapBot(Basetype.Variables)(ValueOriginPair).t

  let update_blame res (x:varinfo) node =
    let node_name: string = PL.show node in
    let curr_list = GobConfig.get_list "ana.path_sens_nodes" in
    if not (List.mem (`String node_name) curr_list) then
      (let _ = GobConfig.set_list "ana.path_sens_nodes" (curr_list @ [`String node_name]) in
       ignore @@ Pretty.printf "Restarting %s\n" node_name;
       (* TODO: Only restart when a warning is produced *)
       raise Refinement.RestartAnalysis;)
    else
      let curr_val_origin_pair = find x res in
      let curr_val = fst curr_val_origin_pair in
      let curr_origin_set = snd curr_val_origin_pair in
      let new_origin: Origin.t = (`Lifted x, node) in
      let new_origin_set = OriginSet.add new_origin curr_origin_set in
      (*ignore @@ Pretty.printf "New OriginSet %s\n\n" (OriginSet.show new_origin_set);*)
      let new_pair = (curr_val, new_origin_set) in 
      let res = add x new_pair res in
      (*ignore @@ Pretty.printf "New Map %s\n\n" (Pretty.sprint 80 (pretty () res));*)
      res

  let check_precision_loss (m1: t) (m2: t) (res: t) node =
    let res = fold (fun (key: Basetype.Variables.t) (v:ValueOriginPair.t) acc -> 
        if (fst (find key res)) <> (fst v) then update_blame acc key node else acc)
        m1 res in 
    let res = fold (fun (key: Basetype.Variables.t) (v:ValueOriginPair.t) acc -> 
        if (fst (find key res)) <> (fst v) then update_blame acc key node else acc)
        m2 res in 
    res

  let join_with_fct f (m1: t) (m2: t) =
    (*let _ = Pretty.printf "JOINING %s %s\n" (Pretty.sprint 80 (pretty () m1)) (Pretty.sprint 80 (pretty () m2)) in*)
    if m1 == m2 then 
      m1 
    else 
      let res = long_map2 f m1 m2 in
      let node = match !MyCFG.current_node with
        | Some n -> `Lifted n
        | _ -> PL.top ()
      in
      check_precision_loss m1 m2 res node

  let join = join_with_fct ValueOriginPair.join

end


(** An analysis that tracks the origin of a value.
    It only considers definite values of local variables.
    We do not pass information interprocedurally. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "origin"

  module D = OriginMap

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.IdentitySpec

  (* Same information for globals as for locals *)
  module G = D
  (* No contexts*)
  module C = Lattice.Unit

  let context _ _ = ()

  let is_pointer_var (v: varinfo) =
    match v.vtype with
    | TPtr _ -> true
    | _ -> false

  let get_pointer = function
    | Var v, NoOffset when is_pointer_var v (* && not v.vglob *)-> Some v (* pointer variable whose address is maybe taken *)
    | _, _ -> None

  (* This is just for debugging *)
  (* let contains s1 s2 =
     let re = Str.regexp_string s2 in
     try ignore (Str.search_forward re s1 0); true
     with Not_found -> false *)

  let should_split node =
    let name: string = Node.show node in
    List.mem name (GobConfig.get_string_list "ana.path_sens_nodes")

  let should_join node x y = 
    match node with
    | Some (n: Node.t) -> not (should_split n)
    | _ -> true

  (** Evaluates expressions *)
  (* let rec eval (state : D.t) (e: exp) (node: PL.t) =
      let addr_val = match e with
      | AddrOf (Var v, _) -> fst (D.find v state)
      | _ -> AD.top ()
      in (addr_val, node) *)

  (* Taken from arinc *)
  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
      let top_elt = (dummyFunDec.svar, `NoOffset) in
      let a' = if Queries.LS.mem top_elt a then (
          M.debug "mayPointTo: query result for %a contains TOP!" d_exp exp; (* UNSOUND *)
          Queries.LS.remove top_elt a
        ) else a
      in
      Queries.LS.elements a'
    | v ->
      M.debug "mayPointTo: query result for %a is %a" d_exp exp Queries.LS.pretty v;
      []
  (*`Top*)
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match get_pointer lval with
    | Some loc -> 
      let curr_val_origin_pair = D.find loc ctx.local in
      let curr_val = fst curr_val_origin_pair in
      let curr_origin_set = snd curr_val_origin_pair in
      let values: Addr.t list = List.map (
          fun x -> 
            let vinfo: varinfo = fst x in 
            Addr.Addr (vinfo, `NoOffset)
        ) (mayPointTo ctx rval) in
      let responsible_source = match rval with
        | Lval (Var v, _) -> v
        | _ -> loc
      in
      let address_set = List.fold_left (fun s (x: Addr.t) -> AD.add x s) curr_val values in
      let new_pair = (address_set, curr_origin_set) in 
      D.add loc new_pair ctx.local
    | None -> ctx.local

  (* TODO: treat the guards as assignments *)
  let branch ctx (exp:exp) (tv:bool) : D.t = ctx.local

  let body ctx (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold (fun m l -> D.add l (AD.empty (), OriginSet.empty ()) m) ctx.local f.slocals

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Do nothing, as we are not interested in return values for now. *)
    let fun_variable = f.svar in
    (* let v_out = Goblintutil.create_var @@ makeVarinfo false (fun_variable.vname ^ "#out") fun_variable.vtype in*) (* temporary local f#out for the output of the function f *)
    (* let node = match !MyCFG.current_node with
       | Some n -> `Lifted n
       | _ -> PL.top ()
       in *)
    (*let new_origin: Origin.t = (`Lifted fun_variable, node) in
      let new_origin_set = OriginSet.add new_origin (OriginSet.empty ())  in*)
    let new_pair = (match exp with
        | Some (e:exp) -> let _ = printf "EXPRESSION: %a\n" (printExp plainCilPrinter) e in
          (match e with
           | AddrOf _ ->  let _ = printf "CASE 1\n" in 
             let possible_values = List.map (
                 fun x -> 
                   let vinfo: varinfo = fst x in
                   let vo_pair = D.find vinfo ctx.local in 
                   (Addr.Addr (vinfo, `NoOffset), vo_pair)
               ) (mayPointTo ctx e) in 
             let newAD = List.fold (fun m x -> AD.add (fst x) m) (AD.empty ()) possible_values in
             let val_origins_from_caller = List.fold (fun s x -> snd(snd x)) (OriginSet.empty ()) possible_values in
             (newAD, val_origins_from_caller)
           | Lval (Var vinfo, _) ->  let _ = printf "CASE 2\n" in  let newAD = AD.add (Addr.Addr (vinfo, `NoOffset)) (AD.empty ()) in 
             let vo_pair = D.find vinfo ctx.local in 
             let neworigin_set = snd vo_pair in
             (newAD, neworigin_set)
           | CastE(_, AddrOf _) -> let _ = printf "CASE 1\n" in 
             let possible_values = List.map (
                 fun x -> 
                   let vinfo: varinfo = fst x in
                   let vo_pair = D.find vinfo ctx.local in 
                   (Addr.Addr (vinfo, `NoOffset), vo_pair)
               ) (mayPointTo ctx e) in 
             let newAD = List.fold (fun m x -> AD.add (fst x) m) (AD.empty ()) possible_values in
             let val_origins_from_caller = List.fold (fun s x -> snd(snd x)) (OriginSet.empty ()) possible_values in
             (newAD, val_origins_from_caller)
           | _ ->  let _ = printf "CASE 3\n" in  (AD.empty (), OriginSet.empty ())
          )
        | _ -> let _ = printf "No value\n" in AD.top () 
      ) in
    let o = D.add fun_variable new_pair ctx.local in
    let _ = Pretty.printf "Return %s\n" (Pretty.sprint 80 (D.pretty () o)) in 
    o 

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Set the formal int arguments to top *)
    (* let _ = Pretty.printf "enter %s\n" (Pretty.sprint 80 (D.pretty () ctx.local)) in *)
    (* let prexp e =
       let _ = printf "Argument: %a\n" (printExp plainCilPrinter) e in
       ()
       in
       let _ = List.iter (fun a -> prexp a) args in *)
    let callee_state = List.fold (fun m l -> D.add l (AD.empty (), OriginSet.empty ()) m) (D.bot ()) f.sformals in
    let callee_state = List.fold_left2 (fun m l1 l2 -> 
        let res = (match (l2:exp) with
            | AddrOf _ -> 
              let possible_values = List.map (
                  fun x -> 
                    let vinfo: varinfo = fst x in
                    let vo_pair = D.find vinfo ctx.local in 
                    (Addr.Addr (vinfo, `NoOffset), vo_pair)
                ) (mayPointTo ctx l2) in 
              let newAD = List.fold (fun m x -> AD.add (fst x) m) (AD.empty ()) possible_values in
              let val_origins_from_caller = List.fold (fun s x -> snd(snd x)) (OriginSet.empty ()) possible_values in
              (newAD, val_origins_from_caller)
            | Lval (Var vinfo, _) -> let newAD = AD.add (Addr.Addr (vinfo, `NoOffset)) (AD.empty ()) in 
              let vo_pair = D.find vinfo ctx.local in 
              let neworigin_set = snd vo_pair in
              (newAD, neworigin_set)
            | _ -> (AD.empty (), OriginSet.empty ()) 
          )
        in
        D.add l1 res m
      ) (D.bot ()) f.sformals args in
    (* let _ = Pretty.printf "new state %s\n" (Pretty.sprint 80 (D.pretty () callee_state)) in *)
    [(ctx.local, callee_state)]

  let set_local_int_lval_to_fun_result (state: D.t) (lval: lval option) (f:fundec) (au:D.t) =
    match lval with
    | Some lv ->
      (match get_pointer lv with
       | Some local -> let _ = Pretty.printf "STATE %s\n" (Pretty.sprint 80 (D.pretty () au)) in D.add local (D.find f.svar au) state
       | _ -> state
      )
    |_ -> state

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* If we have a function call with assignment
        x = f (e1, ... , ek)
        with a local int variable x on the left, we set it to top *)
    (*set_local_int_lval_top ctx.local lval*)
    let _ = Pretty.printf "AU %s\n" (Pretty.sprint 80 (D.pretty () au)) in
    let _ = Pretty.printf "CTX %s\n" (Pretty.sprint 80 (D.pretty () ctx.local)) in
    let _ = List.iter (fun ex ->  ignore @@ printf "Argument: %a\n" (printExp plainCilPrinter) ex;) args in
    let _ = List.iter (fun vinfo ->  ignore @@ printf "SFormal: %s\n" vinfo.vname;) f.sformals in
    let _ = List.iter (fun vinfo ->  ignore @@ printf "SLocall: %s\n" vinfo.vname;) f.slocals in
    match lval with
    | Some (Var v, _) -> let value = D.find f.svar au in
      let op = D.add v value ctx.local in let _ = Pretty.printf "OP %s\n" (Pretty.sprint 80 (D.pretty () op)) in op
    | _ -> ctx.local

  (* set_local_int_lval_to_fun_result ctx.local lval f au *)


  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
