open Batteries
open GoblintCil
open MyCFG
open Analyses
open ConstrSys
open GobConfig

module M = Messages

module NoContext = struct let name = "no context" end

module type Gas = sig
  module M:Lattice.S
  val startgas: unit -> M.t
  val is_exhausted: fundec -> M.t -> bool
  val callee_gas: fundec -> M.t -> M.t
  val thread_gas: varinfo -> M.t -> M.t
end

(** Lifts a [Spec] with the context gas variable. The gas variable limits the number of context-sensitively analyzed function calls in a call stack.
    For every function call the gas is reduced. If the gas is zero, the remaining function calls are analyzed without context-information *)
module ContextGasLifter (Gas:Gas) (S:Spec)
  : Spec with module D = Lattice.Prod (S.D) (Gas.M)
          and module C = Printable.Option (S.C) (NoContext)
          and module G = S.G
=
struct
  include S

  module Context_Gas_Prod (Base1: Lattice.S) (Base2: Lattice.S) =
  struct
    include Lattice.Prod (Base1) (Base2)
    let printXml f (x,y) =
      BatPrintf.fprintf f "\n%a<analysis name=\"context gas value\">\n%a\n</analysis>" Base1.printXml x Base2.printXml y
  end
  module D = Context_Gas_Prod (S.D) (Gas.M) (* Product of S.D and an integer, tracking the context gas value *)
  module C = Printable.Option (S.C) (NoContext)
  module G = S.G
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  (* returns context gas value of the given ctx *)
  let cg_val ctx = snd ctx.local

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize


  let startcontext () = Some (S.startcontext ())
  let name () = S.name ()^" with context gas"
  let startstate v = S.startstate v, Gas.startgas ()
  let exitstate v = S.exitstate v, Gas.startgas ()
  let morphstate v (d,i) = S.morphstate v d, i

  let conv (ctx:(D.t,G.t,C.t,V.t) ctx): (S.D.t,G.t,S.C.t,V.t)ctx =
    {ctx with local = fst ctx.local
            ; split = (fun d es -> ctx.split (d, cg_val ctx) es)
            ; context = (fun () -> match ctx.context () with Some c -> c | None -> ctx_failwith "no context (contextGas = 0)")}

  let context ctx fd (d,i) =
    (* only keep context if the context gas is greater zero *)
    if Gas.is_exhausted fd i then
      None
    else
      Some (S.context (conv ctx) fd d)

  let enter ctx r f args =
    let liftmap_tup = List.map (fun (x,y) -> (x, cg_val ctx), (y, Gas.callee_gas f (cg_val ctx))) in
    liftmap_tup (S.enter (conv ctx) r f args)

  let threadenter ctx ~multiple lval f args =
    let liftmap d = List.map (fun (x) -> (x, Gas.thread_gas f (cg_val ctx))) d in
    liftmap (S.threadenter (conv ctx) ~multiple lval f args)

  let query ctx (type a) (q: a Queries.t):a Queries.result =
    match q with
    | Queries.GasExhausted f ->
      let (d,i) = ctx.local in
      Gas.is_exhausted f i
    | _ -> S.query (conv ctx) q

  let sync ctx reason                             = S.sync (conv ctx) reason, cg_val ctx
  let assign ctx lval expr                        = S.assign (conv ctx) lval expr, cg_val ctx
  let vdecl ctx v                                 = S.vdecl (conv ctx) v, cg_val ctx
  let body ctx fundec                             = S.body (conv ctx) fundec, cg_val ctx
  let branch ctx e tv                             = S.branch (conv ctx) e tv, cg_val ctx
  let return ctx r f                              = S.return (conv ctx) r f, cg_val ctx
  let asm ctx                                     = S.asm (conv ctx), cg_val ctx
  let skip ctx                                    = S.skip (conv ctx), cg_val ctx
  let special ctx r f args                        = S.special (conv ctx) r f args, cg_val ctx
  let combine_env ctx r fe f args fc es f_ask     = S.combine_env (conv ctx) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val ctx
  let combine_assign ctx r fe f args fc es f_ask  = S.combine_assign (conv ctx) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val ctx
  let paths_as_set ctx                            = List.map (fun (x) -> (x, cg_val ctx)) @@ S.paths_as_set (conv ctx)
  let threadspawn ctx ~multiple lval f args fctx  = S.threadspawn (conv ctx) ~multiple lval f args (conv fctx), cg_val ctx
  let event ctx e octx                            = S.event (conv ctx) e (conv octx), cg_val ctx
end

let get_gas_lifter () =
  let module GasChain = Lattice.Chain (struct
      (* Chain lattice has elements [0,n-1], but we want [0,gas_value] *)
      let n () = get_int "ana.context.gas_value" + 1
      let names x = Format.asprintf "%d" x
    end)
  in
  if get_string "ana.context.gas_scope" = "global" then
    let module GlobalGas:Gas = struct
      module M = GasChain
      let startgas () = M.top () (* M.top () yields maximal gas value *)

      let is_exhausted _ v  = v <= 0

      (* callee gas = caller gas - 1 *)
      let callee_gas f v = max 0 (v - 1)
      let thread_gas f v = max 0 (v - 1)
    end
    in
    (module ContextGasLifter(GlobalGas):Spec2Spec)
  else
    let module PerFunctionGas:Gas = struct
      module G = GasChain
      module M = MapDomain.MapTop_LiftBot(CilType.Fundec)(G)
      let startgas () = M.empty ()
      let is_exhausted f v = GobOption.exists (fun g -> g <= 0) (M.find_opt f v)  (* v <= 0 *)
      let callee_gas f v =
        let c = Option.default (G.top ()) (M.find_opt f v) in
        M.add f (max 0 c-1) v
      let thread_gas f v =
        match Cilfacade.find_varinfo_fundec f with
        | fd ->
          callee_gas fd v
        | exception Not_found ->
          callee_gas Cil.dummyFunDec v
    end
    in
    (module ContextGasLifter(PerFunctionGas):Spec2Spec)
