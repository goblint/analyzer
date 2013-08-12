(** Tracking of priorities an partition modes. *)

open Batteries
open Cil
open Pretty
open Analyses

module Pri = 
struct
  module Base = IntDomain.Lifted
  include Base 
  let bot = Base.top
  let is_bot = Base.is_top
  let top = Base.bot
  let is_top = Base.is_bot
  let leq x y = Base.leq y x
  let join x y = Base.meet x y
  let meet x y = Base.join x y
  let name () = "Reversed (" ^ name () ^ ")"
  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
module Pmod = IntDomain.Flattened
module PrE = IntDomain.Flattened

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "arinc"
  module D = 
  struct 
    include Lattice.Prod3 (Pri) (Pmod) (PrE)
    let toXML_f sf (p,m,e) = 
      let replace_top name = function
          | Xml.Element (node, [text, n], elems) -> Xml.Element (node, [text, name ^ n], elems)
          | x -> x
      in
      let elems = [ replace_top "Priority: "   @@ Pri.toXML  p
                  ; replace_top "Part-mode: "  @@ Pmod.toXML m
                  ; replace_top "Preemption lock: " @@ PrE.toXML  e ] in
      Xml.Element ("Node", ["text", "ARINC state"], elems)
      
    let toXML s  = toXML_f short s
  end
  module G = IntDomain.Booleans
  module C = D
  
  let is_single ctx =
    let fl : BaseDomain.Flag.t = snd (Obj.obj (List.assoc "base" ctx.presub)) in
    not (BaseDomain.Flag.is_multi fl)

  let part_mode_var = makeGlobalVar "__GOBLINT_ARINC_MUTLI_THREADED" voidPtrType 
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    if not (is_single ctx || !Goblintutil.global_initialization || ctx.global part_mode_var) then raise Analyses.Deadcode;
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au
  
  let delayed_create_process = ref []
  let create_process ctx lval arglist =
    if M.tracing then M.tracel "arinc" "found LAP_Se_CreateProcess\n";
    match List.hd arglist with
      | Lval lv -> begin
        let cm  = 
          match unrollType (typeOfLval lv) with
            | TComp (c,_) -> c
            | _ -> failwith "type-error: first arg. of LAP_Se_CreateProcess not a struct."
        in
        let ofs = Field (getCompField cm Goblintutil.arinc_base_priority, NoOffset) in
        match ctx.ask (Queries.EvalInt (Lval (addOffsetLval ofs lv)))
            , ctx.ask (Queries.ReachableFrom (AddrOf lv)) with
          | `Int i, `LvalSet ls when not (Queries.LS.is_top ls) 
                                  && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) -> 
              let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls in
              if M.tracing then M.tracel "arinc" "starting a threads %a with priority '%Ld' \n" Queries.LS.pretty funs i;
              Queries.LS.iter (fun f -> ctx.spawn (fst f) (Pri.of_int i, Pmod.of_int 3L, PrE.of_int 0L)) funs;
              ctx.local
          | `Bot, _ | _, `Bot -> D.bot ()
          | _ -> ctx.local
        end
      | _ -> ctx.local
    
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match f.vname with
      | "LAP_Se_LockPreemption" -> begin
          if M.tracing then M.tracel "arinc" "found LAP_Se_LockPreemption\n";
          let p,m,e = ctx.local in
          (p,m,PrE.add e (PrE.of_int 1L))
        end
      | "LAP_Se_UnlockPreemption" -> begin
          if M.tracing then M.tracel "arinc" "found LAP_Se_UnlockPreemption\n";
          let p,m,e = ctx.local in
          (p,m,PrE.sub e (PrE.of_int 1L))
        end
      | "LAP_Se_SetPartitionMode" -> begin
          if M.tracing then M.tracel "arinc" "found LAP_Se_SetPartitionMode\n";
          let p,m,e = ctx.local in
          match ctx.ask (Queries.EvalInt (List.hd arglist)) with
            | `Int i -> 
              if M.tracing then M.tracel "arinc" "setting partition mode to %Ld\n" i;
              if i<>1L && i<>2L then ctx.sideg part_mode_var true;
              (p,Pmod.of_int i,e)
            | `Bot -> D.bot ()
            | _ -> ctx.sideg part_mode_var true; D.top ()
        end
      | "LAP_Se_CreateProcess" -> begin
        if M.tracing then M.tracel "arinc" "found LAP_Se_CreateProcess\n";
        let farg = stripCasts (List.hd arglist) in
        match farg with
          | AddrOf lv -> begin
            let cm  = 
              match unrollType (typeOfLval lv) with
                | TComp (c,_) -> c
                | _ -> failwith "type-error: first arg. of LAP_Se_CreateProcess not a struct."
            in
            let ofs = Field (getCompField cm Goblintutil.arinc_base_priority, NoOffset) in
            let pri = 
              match ctx.ask (Queries.EvalInt (Lval (addOffsetLval ofs lv))) with
                | `Int i -> Pri.of_int i 
                | _ -> Pri.top ()
            in
            let ofs' = Field (getCompField cm Goblintutil.arinc_entry_point, NoOffset) in
            match ctx.ask (Queries.MayPointTo (Lval (addOffsetLval ofs' lv))) with
              | `LvalSet ls when not (Queries.LS.is_top ls) 
                                      && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) -> 
                  let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls in
                  if M.tracing then M.tracel "arinc" "starting a threads %a with priority '%a' \n" Queries.LS.pretty funs Pri.pretty pri;
                  Queries.LS.iter (fun f -> ctx.spawn (fst f) (pri, Pmod.of_int 3L, PrE.of_int 0L)) funs;
                  ctx.local
              | `Bot -> D.bot ()
              | _ -> ctx.local
              
            end
          | _ -> ctx.local
        end
      | _ -> ctx.local

  let query ctx (q:Queries.t) : Queries.Result.t = 
    let p,m,e = ctx.local in
    match q with
      | Queries.Priority _ -> 
          if Pri.is_int p then 
            `Int (Option.get @@ Pri.to_int p) 
          else if Pri.is_top p then `Top else `Bot
      | Queries.IsPrivate _ ->
          `Bool ((PrE.to_int e <> Some 0L && PrE.to_int e <> None) || Pmod.to_int m = Some 1L || Pmod.to_int m = Some 2L)
      | _ -> Queries.Result.top ()

  let startstate v = (Pri.top (), Pmod.of_int 1L, PrE.of_int 0L)
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = 
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)
